package org.scalawag.druthers

import scala.language.postfixOps

import scala.reflect.runtime.universe._
import scala.util.Try
import org.scalawag.timber.api.style.slf4j
import scala.util.Failure
import scala.util.Success
import java.io.PrintWriter
import scala.annotation.tailrec
import org.scalawag.druthers.Parser._

object OptionsParser {

  object OptionSpec {
    object Type extends Enumeration {
      val STRING = Value
      val INTEGER = Value
      val FLOAT = Value
      val BOOLEAN = Value
      val COUNTER = Value
    }
  }

  case class OptionSpec(key:String,
                        name:String,
                        argType:OptionSpec.Type.Value,
                        cardinality:Cardinality.Value,
                        usage:Option[String] = None,
                        default:Option[Any] = None) {
    def requiresValue = argType match {
      case OptionSpec.Type.BOOLEAN => false
      case OptionSpec.Type.COUNTER => false
      case _ => true
    }
  }

  private val WordGrabber = "( *)([^ ]+)(.*)".r
  private val CharPopper = "(.)(.*)".r
  private val NoPrefix = "no-(.*)".r
}

class OptionsParser[C:TypeTag](cfg:ParserConfiguration = ShortOptions(),classLoader:Option[ClassLoader] = None) extends Parser[C](classLoader) {
  import OptionsParser._
  import OptionSpec.Type
  import Logging.log

  val (specs:List[OptionSpec],specMap) = {

    val specsWithoutDefaults:List[OptionSpec] = getParams map { param =>
      val name = param.name.toString
      val typeSignature = param.typeSignatureIn(typeOf[C])

      val usage = getUsage(param)

      val (cardinality,valueTypeSignature) = getCardinalityAndValueType(typeSignature)

      val valueType =
        if ( valueTypeSignature =:= STRING_TYPE )
          Type.STRING
        else if ( valueTypeSignature =:= INTEGER_TYPE )
          Type.INTEGER
        else if ( valueTypeSignature =:= FLOAT_TYPE )
          Type.FLOAT
        else if ( valueTypeSignature =:= BOOLEAN_TYPE && cardinality == Cardinality.REQUIRED )
          Type.BOOLEAN
        else if ( valueTypeSignature =:= COUNTER_TYPE && cardinality == Cardinality.REQUIRED )
          Type.COUNTER
        else
          throw new IllegalArgumentException(s"unsupported constructor parameter type for '$name': $typeSignature")


      val key =
        if ( cfg.useLongKeys )
          uncamelCase(name)
        else
          name.substring(0,1)

      OptionSpec(key,name,valueType,cardinality,usage)
    }

    val specsWithDefaults = specsWithoutDefaults.zip(getParameterDefaults) map { case (spec,default) =>
      spec.copy(default = default)
    }

    val specMap = specsWithDefaults.map( f => f.key -> f ).toMap

    if ( specMap.size < specsWithDefaults.size ) {
      val collisions = specsWithDefaults.groupBy(_.key).flatMap { case (key,colliders) =>
        if ( colliders.length > 1 )
          Some(s"  $key => ${colliders.map(_.name).mkString(" ")}")
        else
          None
      }.toSeq
      val lines = "key collisions found, use long keys or change your field names" +: collisions
      throw new IllegalArgumentException(lines.mkString("","\n","\n"))
    }

    (specsWithDefaults,specMap)
  }

  private[this] case class Parse(values:Map[OptionSpec,Any] = Map.empty,
                                 bareWords:List[String] = Nil,
                                 errors:Seq[UsageError] = Nil) {

    private[this] def setValue(spec:OptionSpec,value:Any) = {
      copy(values = values + ( spec -> value ))
    }

    private[this] def incrementValue(spec:OptionSpec) =
      values.get(spec) match {
        case None =>
          setValue(spec,1)
        case Some(existing) =>
          setValue(spec,existing.asInstanceOf[Int] + 1)
    }

    private def addValue(spec:OptionSpec,value:Any) = {
      values.get(spec) match {
        case None => spec.cardinality match {
            case Cardinality.MULTIPLE =>
              setValue(spec,Seq(value))
            case Cardinality.OPTIONAL =>
              setValue(spec,Some(value))
            case Cardinality.REQUIRED =>
              setValue(spec,value)
          }

        case Some(existing) => spec.cardinality match {
          case Cardinality.MULTIPLE =>
            setValue(spec,existing.asInstanceOf[Seq[Any]] :+ value)
          case _ =>
            addError(DuplicateValue(spec,existing,value))
        }
      }
    }

    private def addConvertedValue(spec:OptionSpec,stringValue:String,converter:String => Any) = {
      Try(converter(stringValue)) match {
        case Success(value) =>
          addValue(spec,value)
        case Failure(ex) =>
          // We just need a value to prevent a MissingKey error as well.  It doesn't have to be the right type
          // because, with errors, we'll never get as far as trying to cast them to the right type to call
          // the constructor.
          addError(InvalidValue(spec,stringValue,ex.getLocalizedMessage)).addValue(spec,stringValue)
      }
    }

    def consumeSpec(spec:OptionSpec,key:String):Parse = consumeSpec(spec,Some(key))

    def consumeSpec(spec:OptionSpec,key:Option[String] = None):Parse =
      spec.argType match {
        case Type.BOOLEAN =>
          // It's kind of weird to handle the negation here but it's keeps the rest of the code cleaner.
          // Almost none of it cares about the value implied by the key that was used to find the spec.
          val v = key match {
            // The only implication is that if the key specified without a "no-" prefix matches the real
            // key for the spec, the implied value is "false."  Otherwise, the implied value is true
            case Some(NoPrefix(realKey)) if spec.key startsWith realKey => false
            case _ => true
          }
          addValue(spec,v)
        case Type.COUNTER =>
          incrementValue(spec)
        case _ =>
          // We just need a value to prevent a MissingKey error as well.  It doesn't have to be the right type
          // because, with errors, we'll never get as far as trying to cast them to the right type to call
          // the constructor.
          addError(MissingValue(spec)).addValue(spec,false)
      }

    def consumeSpecAndValue(spec:OptionSpec,stringValue:String) =
      spec.argType match {
        case Type.BOOLEAN => addError(UnexpectedValue(spec,stringValue))
        case Type.COUNTER => addError(UnexpectedValue(spec,stringValue))
        case _ =>
          val converter:(String => Any) = spec.argType match {
            case Type.STRING => { s:String => s }
            case Type.INTEGER => { s:String => s.toInt }
            case Type.FLOAT => { s:String => s.toFloat }
            case at => throw new IllegalStateException(s"internal error: spec $spec should never use this method")
          }

          cfg.valueDelimiter match {
            case Some(delim) =>
              stringValue.split(delim).filter(_.length > 0 ).foldLeft(this) { case (me,individual) =>
                me.addConvertedValue(spec,individual,converter)
              }
            case None =>
              addConvertedValue(spec,stringValue,converter)
          }
      }

    def consumeBareWords(words:Seq[String]) = this.copy(bareWords = bareWords ++ words)

    def consumeBareWord(word:String) = this.copy(bareWords = bareWords :+ word)

    def addError(error:UsageError) = this.copy(errors = errors :+ error)
  }

  private[this] def buildConstructorArgs(parse:Parse):(Parse,List[Any]) = {

    def helper(parse:Parse,specs:List[OptionSpec],constructorArgs:List[Any]):(Parse,List[Any]) =
      specs match {
        case Nil =>
          (parse,constructorArgs)
        case spec :: tail =>
          val value = parse.values.get(spec)

          def continue(arg:Any) = helper(parse,tail,arg :: constructorArgs)
          def fail(error:UsageError) = helper(parse.copy(errors = parse.errors :+ error),tail,null :: constructorArgs)

          spec.argType match {
            case Type.BOOLEAN => continue(firstDefinedOf(value,spec.default).getOrElse(false))
            case Type.COUNTER => continue(Counter(value.getOrElse(0).asInstanceOf[Int]))
            case _ => spec.cardinality match {
              case Cardinality.MULTIPLE => continue(firstDefinedOf(value,spec.default).getOrElse(Nil))
              case Cardinality.OPTIONAL => continue(firstDefinedOf(value,spec.default).getOrElse(None))
              case Cardinality.REQUIRED =>
                firstDefinedOf(value,spec.default) match {
                  case Some(v) => continue(v)
                  case None => fail(MissingRequiredKey(spec))
                }
            }
          }
      }

    helper(parse,specs.reverse,Nil)
  }

  // Used to match args against the option prefix and remove it

  private[this] object OptionPrefix {
    def unapply(arg:String) =
      if ( arg.startsWith(cfg.optionPrefix) && arg.length > cfg.optionPrefix.length )
        Some(arg.substring(cfg.optionPrefix.length))
      else None
  }

  private[this] def withKey(key:String):Either[OptionSpec,UsageError] = {

    val matchingSpecs = firstNonEmptyOf(
      // If there's an exact match, look no further.  Consider only this one.
      specMap.get(key).toSeq,
      // If "no-" is enabled and there's an exact match (that's a boolean), look no further.
      key match {
        case NoPrefix(realKey) if cfg.booleansNegatedByNoPrefix =>
          specMap.get(realKey).filter(_.argType == Type.BOOLEAN).toSeq
        case _ => Nil
      },
      // If we couldn't find an exact match, look for anything that starts with the string passed in (or the
      // negated version, if that's allowed).
      if ( cfg.abbreviations ) {
        val matchingPositives = specs.filter(_.key.startsWith(key))
        val matchingNegatives = key match {
          case NoPrefix(realKey) if cfg.booleansNegatedByNoPrefix =>
            specs.filter(_.argType == Type.BOOLEAN).filter(_.key.startsWith(realKey))
          case _ => Nil
        }
        matchingPositives ++ matchingNegatives
      } else {
        Nil
      }
    )

    matchingSpecs match {
      case Seq(spec) =>
        Left(spec)
      case Nil =>
        Right(UnknownKey(cfg.optionPrefix + key))
      case all =>
        Right(AmbiguousKey(cfg.optionPrefix + key,all))
    }
  }

  private[this] def parseRec(args:List[String],parse:Parse = Parse()):Parse = {
    log.debug(s"PARSE: $args $parse")

    args match {

      case Nil =>
        log.debug(s"No more args, returning")
        parse

      case OptionPrefix(argHead) :: argTail =>
        def consumeOneArg(p:Parse) = parseRec(argTail,p)
        def consumeTwoArgs(p:Parse) = parseRec(argTail.tail,p)

        // found an arg that starts with the option prefix
        if ( cfg.useLongKeys ) {

          // Try to separate it into key and value at '=' if that's possible (and we're allowed)

          val collapsed =
            if ( cfg.mayCollapseValues ) {
              val p = argHead.split("=",2) match {

                // arg is separable into key and value with '='
                case Array(key,value) =>
                  Some(withKey(key) match {
                    case Left(spec) => parse.consumeSpecAndValue(spec,value)
                    case Right(error) => parse.addError(error)
                  })

                // arg contains no '=' and it's required
                case Array(key) if cfg.mustCollapseValues =>
                  Some(withKey(key) match {
                    case Left(spec) => parse.consumeSpec(spec,key)
                    case Right(error) => parse.addError(error)
                  })

                // This is not a case of a collapsed value (nor was it required to be).
                // 'None' here signals that getOrElse block below should handle this arg.
                case _ => None

              }

              p.map(consumeOneArg) // each of the above blocks consumes exactly one arg
            } else {
              None
            }

          // If collapsing is not required or this arg didn't contain an '=' (the above block
          // didn't return a next parse), just handle the arg as a key and use the next arg as
          // the value.

          collapsed.getOrElse {
            withKey(argHead) match {
              case Left(spec) =>
                if ( ! spec.requiresValue || argTail.isEmpty || argTail.head.startsWith(cfg.optionPrefix) ) {
                  val p = parse.consumeSpec(spec,argHead)
                  consumeOneArg(p)
                } else {
                  val p = parse.consumeSpecAndValue(spec,argTail.head)
                  consumeTwoArgs(p)
                }
              case Right(error) =>
                val p = parse.addError(error)
                consumeOneArg(p)
            }
          }

        } else { // use short keys

          argHead match {
            case CharPopper(charHead,charTail) =>

              def consumeOneLetterOnly(p:Parse) = {
                // Fake this out so that the parser will think that the rest of this argument was the
                // start of a new option.
                val manufacturedArgs = ( cfg.optionPrefix + charTail ) +: argTail
                parseRec(manufacturedArgs,p)
              }

              withKey(charHead) match {

                case Left(spec) =>
                  if ( charTail.isEmpty ) {
                    // There's nothing following the key in this arg.  There either has to be no value or the value
                    // lives in the next argument.
                    if ( !spec.requiresValue || cfg.mustCollapseValues || argTail.isEmpty || argTail.head.startsWith(cfg.optionPrefix) ) {
                      val p = parse.consumeSpec(spec)
                      consumeOneArg(p)
                    } else {
                      val p = parse.consumeSpecAndValue(spec,argTail.head)
                      consumeTwoArgs(p)
                    }
                  } else {
                    if ( cfg.mayCollapseValues && ( spec.requiresValue || !cfg.clustering ) ) {
                      val p = parse.consumeSpecAndValue(spec,charTail)
                      consumeOneArg(p)
                    } else {
                      val p = parse.consumeSpec(spec)
                      consumeOneLetterOnly(p)
                    }
                  }

                case Right(error) =>
                  val p = parse.addError(error)
                  consumeOneLetterOnly(p)

              }
          }
        }

      case rest if cfg.stopAtFirstBareWord =>
        parse.consumeBareWords(rest)

      case argHead :: argTail =>
        val p = parse.consumeBareWord(argHead)
        parseRec(argTail,p) // consume one arg
    }
  }

  def parse(args:List[String]):(C,List[String]) = {

    // Parse and attempt to extract a list of constructor arguments from the results

    val (parse,constructorArgs) = buildConstructorArgs(parseRec(args))

    log.debug { pw:PrintWriter =>
      pw.println("OPTIONS PARSER RESULTS:")
      pw.println("  VALUES:")
      ( specs zip constructorArgs) foreach { case (f,ca) =>
        pw.println(s"    $f => $ca")
      }
      pw.println("  ERRORS:")
      parse.errors foreach { e =>
        pw.println(s"    $e")
      }
    }

    if ( ! parse.errors.isEmpty )
      throw new UsageException(parse.errors)

    (instantiate(constructorArgs),parse.bareWords)
  }

  def parse(args:Array[String]):(C,List[String]) = parse(args.toList)

  def usage(totalWidth:Int = 120,indent:Int = 2,gap:Int = 6) = {
    val column1 = specs map { f =>
      Seq(
        Seq(cfg.optionPrefix,f.key),
        f.requiresValue match {
          case false => Nil
          case true =>
            Seq(
              " <",
              if ( cfg.useLongKeys ) {
                f.argType match {
                  case Type.STRING  => "s"
                  case Type.INTEGER => "n"
                  case Type.FLOAT   => "f"
                }
              } else {
                f.name
              },
              ">"
            )
        }
      ).flatten.mkString
    }

    val c1width = column1.map(_.length).max
    val c2start = indent + c1width + gap
    val c2width = totalWidth - c2start

    val column2 = specs map { f =>
      val extra = f.cardinality match {
        case Cardinality.MULTIPLE => Some(" (repeatable)")
        case Cardinality.REQUIRED => Some(" (required)")
        case _ => None
      }

      wordWrap(Seq(f.usage,extra).flatten.mkString,c2width)
    }

    val c1format = s"%${indent}s%-${c1width}s%${gap}s%s".format("",_:String,"",_:String)
    val c2format = s"%${c2start}s%s".format("",_:String)

    column1.zip(column2) flatMap { case (c1,c2s) =>
      c1format(c1,c2s.head) +: c2s.tail.map(c2format)
    }
  }

  private def wordWrap(text:String,width:Int) = {
    @tailrec
    def helper(in:String,lines:Seq[String] = Nil):Seq[String] = in match {
      case WordGrabber(space,word,rest) =>
        lines match {
          case Nil =>
            helper(rest,Seq(word))
          case _ =>
            if ( ( space.length + word.length + lines.head.length ) > width )
              helper(rest,word +: lines)
            else
              helper(rest,( lines.head + space + word ) +: lines.tail)
        }
      case _ => lines
    }

    helper(text).reverse
  }

  def unapply(args:List[String]):Option[(C,List[String])] =
    try {
      Some(parse(args))
    } catch {
      case ex:UsageException if cfg.quietMode => None
    }

  def unapply(args:Array[String]):Option[(C,List[String])] = unapply(args.toList)

  private def uncamelCase(s:String) = s flatMap { c =>
    if ( c.isUpper )
      Seq('-',c.toLower)
    else
      Seq(c)
  }

  private def firstDefinedOf[A](items:Option[A]*) = items.flatten.headOption
  private def firstNonEmptyOf[A](items:Seq[A]*) = items.find(!_.isEmpty).getOrElse(Nil)
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
