package org.scalawag.druthers

import scala.reflect.runtime.universe._
import scala.util.Try
import org.scalawag.timber.api.style.slf4j
import scala.util.Failure
import scala.util.Success
import java.io.PrintWriter
import scala.annotation.tailrec

object OptionsParser {
  case class OptionSpec(key:String,
                        name:String,
                        argType:ArgType.Value,
                        cardinality:Cardinality.Value,
                        usage:Option[String]) {
    def requiresValue = argType match {
      case ArgType.BOOLEAN => false
      case ArgType.COUNTER => false
      case _ => true
    }
  }

  object Cardinality extends Enumeration {
    val OPTIONAL = Value
    val REQUIRED = Value
    val MULTIPLE = Value
  }

  object ArgType extends Enumeration {
    val STRING = Value
    val INTEGER = Value
    val FLOAT = Value
    val BOOLEAN = Value
    val COUNTER = Value
  }

  private val SEQUENCE_TYPE = typeOf[Seq[Any]]
  private val OPTION_TYPE = typeOf[Option[Any]]
  private val STRING_TYPE = typeOf[String]
  private val INTEGER_TYPE = typeOf[Int]
  private val FLOAT_TYPE = typeOf[Float]
  private val BOOLEAN_TYPE = typeOf[Boolean]
  private val COUNTER_TYPE = typeOf[Counter]

  private val USAGE_TYPE = typeOf[Usage]
  private val VALUE_TERM = newTermName("value")

  private val NOTHING_TYPE = typeOf[Nothing]

  private val WordRE = "( *)([^ ]+)(.*)".r
}

class OptionsParser[C:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends slf4j.Logging {
  import OptionsParser._

  // Identifies the specs implied by the container class.

  val (specs,specMap) = {

    val params = constructor.paramss match {
      case Seq(head) => head
      case _ =>
        throw new IllegalArgumentException("target class constructor takes no arguments, making it not a very useful option container")
    }

    val specs = params map { param =>
      val name = param.name.toString
      val typeSignature = param.typeSignatureIn(typeOf[C])

      val (cardinality,argTypeSignature) =
        if ( typeSignature.erasure <:< SEQUENCE_TYPE )
          (Cardinality.MULTIPLE,typeSignature.asInstanceOf[TypeRef].args.head)
        else if ( typeSignature.erasure <:< OPTION_TYPE )
          (Cardinality.OPTIONAL,typeSignature.asInstanceOf[TypeRef].args.head)
        else
          (Cardinality.REQUIRED,typeSignature)

      val argType =
        if ( argTypeSignature <:< STRING_TYPE )
          ArgType.STRING
        else if ( argTypeSignature <:< INTEGER_TYPE )
          ArgType.INTEGER
        else if ( argTypeSignature <:< FLOAT_TYPE )
          ArgType.FLOAT
        else if ( argTypeSignature <:< BOOLEAN_TYPE && cardinality == Cardinality.REQUIRED )
          ArgType.BOOLEAN
        else if ( argTypeSignature <:< COUNTER_TYPE && cardinality == Cardinality.REQUIRED )
          ArgType.COUNTER
        else
          throw new IllegalArgumentException(s"unsupported constructor parameter type for '$name': $typeSignature")

      val usage = param.annotations.find(_.tpe =:= USAGE_TYPE).flatMap(_.javaArgs.get(VALUE_TERM)).map {
        case LiteralArgument(Constant(s:String)) => s
      }

      val key =
        if ( cfg.useLongKeys )
          uncamelCase(name)
        else
          name.substring(0,1)

      OptionSpec(key,name,argType,cardinality,usage)
    }

    val specMap = specs.map( f => f.key -> f ).toMap

    if ( specMap.size < specs.size ) {
      val collisions = specs.groupBy(_.key).flatMap { case (key,specs) =>
        if ( specs.length > 1 )
          Some(s"  $key => ${specs.map(_.name).mkString(" ")}")
        else
          None
      }.toSeq
      val lines = "key collisions found, use LongKeys or change your field names" +: collisions
      throw new IllegalArgumentException(lines.mkString("","\n","\n"))
    }

    (specs,specMap)
  }

  private lazy val constructor = {

    if ( typeOf[C] <:< NOTHING_TYPE )
      throw new IllegalArgumentException(s"target class not specified, add a type parameter to OptionsParser")

    val constructors = typeOf[C].declarations.collect {
      case m:MethodSymbol if m.isConstructor => m
    }.toSeq

    constructors match {
      case Seq(only) => only
      case seq =>
        throw new IllegalArgumentException(s"target class must have exactly one constructor, yours (${typeOf[C].typeSymbol.name}) has ${seq.length}")
    }

  }

  def parseInternal(args:List[String]) = {
    var remainingArgs = args
    var inOptionCluster = false
    var currentSpec:Option[OptionSpec] = None
    var values:Map[OptionSpec,Any] = Map.empty
    var bareWords:List[String] = Nil
    var errors:List[UsageError] = Nil

    def incrementValue(spec:OptionSpec) =
      values.get(spec) match {
        case None =>
          values += ( spec -> 1 )
        case Some(existing) =>
          values += ( spec -> ( existing.asInstanceOf[Int] + 1 ) )
    }

    def addValue(spec:OptionSpec,value:Any) {
      values.get(spec) match {
        case None => spec.cardinality match {
          case Cardinality.MULTIPLE =>
            values += ( spec -> Seq(value) )
          case Cardinality.OPTIONAL =>
            values += ( spec -> Some(value) )
          case Cardinality.REQUIRED =>
            values += ( spec -> value )
        }

        case Some(existing) => spec.cardinality match {
          case Cardinality.MULTIPLE =>
            values += ( spec -> ( existing.asInstanceOf[Seq[Any]] :+ value ) )
          case _ =>
            errors :+= DuplicateValue(spec,existing,value)
        }
      }
    }

    def addConvertedValue(spec:OptionSpec,stringValue:String,converter:String => Any) {
      Try(converter(stringValue)) match {
        case Success(value) =>
          addValue(spec,value)
        case Failure(ex) =>
          errors :+= InvalidValue(spec,stringValue,ex.getLocalizedMessage)
      }
    }

    def addStringValue(spec:OptionSpec,stringValue:String) {
      val converter:(String => Any) = spec.argType match {
        case ArgType.STRING => { s:String => s }
        case ArgType.INTEGER => { s:String => s.toInt }
        case ArgType.FLOAT => { s:String => s.toFloat }
        case at => throw new IllegalStateException(s"internal error: spec $spec should never use this method")
      }

      cfg.valueDelimiter match {
        case Some(delim) =>
          stringValue.split(delim).filter(_.length > 0 ).foreach { individual =>
            addConvertedValue(spec,individual,converter)
          }
        case None =>
          addConvertedValue(spec,stringValue,converter)
      }
    }

    val NoPrefixRE = "no-(.*)".r

    def getSpecs(key:String):Seq[OptionSpec] = {
      specMap.get(key) match {
        case Some(spec) =>
          Seq(spec)
        case None =>
          if ( cfg.abbreviations ) {
            specs.filter(_.key.startsWith(key))
          } else {
            Nil
          }
      }
    }

    // returns success
    def forSpec(key:String)(fn:OptionSpec => Unit) = {
      finishKey
      getSpecs(key) match {
        case Seq(spec) =>
          fn(spec)
          true
        case Nil =>
          errors :+= UnknownKey(cfg.optionPrefix + key)
          false
        case all =>
          errors :+= AmbiguousKey(cfg.optionPrefix + key,all)
          false
      }
    }

    def forNoSpec(noKey:String) = {
      finishKey
      noKey match {
        case NoPrefixRE(key) if cfg.booleansNegatedByNoPrefix =>
          getSpecs(key).filter(_.argType == ArgType.BOOLEAN) match {
            case Nil =>
              false
            case Seq(spec) =>
              addValue(spec,false)
              true
            case all =>
              errors :+= AmbiguousKey(cfg.optionPrefix + noKey,all)
              true
          }
        case _ =>
          false
      }
    }

    def consumeKeyAndValue(spec:OptionSpec,value:String) {
      finishKey

      spec.argType match {
        case ArgType.BOOLEAN =>
          errors :+= UnexpectedValue(spec,value)
        case ArgType.COUNTER =>
          errors :+= UnexpectedValue(spec,value)
        case _ =>
          addStringValue(spec,value)
      }
    }

    def consumeKey(spec:OptionSpec) {
      finishKey

      spec.argType match {
        case ArgType.BOOLEAN =>
          log.debug(s"Got boolean key ${spec.key}, adding true value")
          addValue(spec,true)
        case ArgType.COUNTER =>
          log.debug(s"Got counter key ${spec.key}, incrementing value")
          incrementValue(spec)
        case _ =>
          if ( ! cfg.useLongKeys && cfg.mustCollapseValues ) {
            errors :+= MissingValue(spec)
          } else {
            log.debug(s"Got key ${spec.key}, waiting for value(s)")
            currentSpec = Some(spec)
          }
      }
    }

    def finishKey {
      // Raise an error if the last spec never got its value(s)
      currentSpec map { cf =>
        errors :+= MissingValue(cf)
      }
      // Mark that we're able to start a new spec now
      currentSpec = None
    }

    var done = false
    // max number of iterations we'll allow (guarding against infinite loops)
    var iters = args.map(_.length).fold(10)(_+_)

    def checkForInfiniteLoop {
      iters -= 1
      if ( iters < 0 )
        throw new IllegalStateException("This appears to be a bug in druthers, seems like an infinite loop.")
    }

    while ( ! done ) {
      log.debug("PARSE: " + remainingArgs.mkString(" "))
      currentSpec match {
        case None => remainingArgs match {

          case Nil =>
            log.debug(s"No more tokens, returning")
            done = true

          case head :: tail if head.startsWith(cfg.optionPrefix) && head.length > cfg.optionPrefix.length =>
            val bareHead = head.substring(cfg.optionPrefix.length)

            // found an arg that starts with the option prefix
            if ( cfg.useLongKeys ) {
              // try to separate it into key and value at '=' if that's possible
              if ( cfg.mayCollapseValues ) {
                bareHead.split("=",2) match {
                  // arg is separable into key and value with '='
                  case Array(key,value) =>
                    forSpec(key)(consumeKeyAndValue(_,value))
                  // arg contains no '=' and it's required
                  case Array(key) if cfg.mustCollapseValues =>
                    forSpec(key) { spec =>
                      errors :+= MissingValue(spec)
                    }
                  // arg contains no '=' but we didn't need it (it was optional)
                  case Array(key) =>
                    forNoSpec(bareHead) || forSpec(key)(consumeKey)
                }
              } else {
                // long key must appear in an arg by itself (no '=', no value)
                forNoSpec(bareHead) || forSpec(bareHead)(consumeKey)
              }
            } else { // use short keys
              // This var will keep track of the rest of the arg we still have to parse
              var rest = bareHead

              while ( ! rest.isEmpty ) {
                log.debug("REST: " + rest)
                // Look at the first character to get the (maybe first) key
                var k = rest.substring(0,1)
                rest = rest.tail

                forSpec(k) { spec =>
                  // This expression basically just says that, if there's more stuff following
                  // the key and we're allow to COLLAPSE TODO the key is expecting a value or clustering is disabled, then treat the
                  // following stuff as the value for the key.  Otherwise, consume the key
                  // and wait for its value to follow.  Note that, in some cases, this will
                  // cause an error to be raised but that's exactly what should happen in
                  // those cases.
                  if ( !rest.isEmpty && cfg.mayCollapseValues && ( spec.requiresValue || !cfg.clustering ) ) {
                    consumeKeyAndValue(spec,rest)
                    rest = ""
                  } else {
                    consumeKey(spec)
                  }
                }

                checkForInfiniteLoop
              }
            }
            remainingArgs = tail

          case head :: tail if currentSpec.isDefined =>
            // TODO: handle other cases like multiple appearances set multiple values
            addStringValue(currentSpec.get,head)
            currentSpec = None
            remainingArgs = tail

          case head :: tail if inOptionCluster =>
            // First character should be treated as a option key
            forSpec(head.head.toString)(consumeKey)

          case head :: tail if cfg.stopAtFirstBareWord =>
            done = true

          case head :: tail =>
            bareWords :+= head
            remainingArgs = tail
        }

        case Some(spec) => remainingArgs match {
          case Nil =>
            finishKey

          case head :: tail if head.startsWith(cfg.optionPrefix) =>
            finishKey

          case head :: tail =>
            log.debug(s"Collecting value $head for spec $spec")
            addStringValue(spec,head)
            currentSpec = None
            remainingArgs = tail
        }
      }

      checkForInfiniteLoop
    }

    if ( ! errors.isEmpty ) {
      throw new UsageException(errors)
    } else {
      (values,bareWords ++ remainingArgs)
    }
  }

  def parse(args:Array[String]):(C,List[String]) = parse(args.toList)

  def parse(args:List[String]):(C,List[String]) = {
    val (valuesMap,remains) = parseInternal(args)

    // TODO: combine this with the one in parseInternal for one list of errors
    var errors:List[UsageError] = Nil

    val constructorArgs = specs map { spec =>
      val value = valuesMap.get(spec)

      spec.argType match {
        case ArgType.BOOLEAN => value.getOrElse(false)
        case ArgType.COUNTER => Counter(value.getOrElse(0).asInstanceOf[Int])
        case _ => spec.cardinality match {
          case Cardinality.MULTIPLE => value.getOrElse(Nil)
          case Cardinality.OPTIONAL => value.getOrElse(None)
          case Cardinality.REQUIRED =>
            value match {
              case Some(v) =>
                v
              case None =>
                errors :+= MissingRequiredKey(spec)
            }
        }
      }
    }

    log.debug { pw:PrintWriter =>
      pw.println("PARSE RESULTS:")
      ( specs zip constructorArgs) foreach { case (f,ca) =>
        pw.println(s"  $f => $ca")
      }
    }

    if ( ! errors.isEmpty )
      throw new UsageException(errors)

    val mirror = runtimeMirror(Thread.currentThread.getContextClassLoader)
    val classSymbol = typeOf[C].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val constructorMirror = classMirror.reflectConstructor(constructor)
    (constructorMirror.apply(constructorArgs:_*).asInstanceOf[C],remains)
  }

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
                  case ArgType.STRING  => "s"
                  case ArgType.INTEGER => "n"
                  case ArgType.FLOAT   => "f"
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
      case WordRE(space,word,rest) =>
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

  def unapply(args:Array[String]):Option[(C,List[String])] = unapply(args.toList)

  def unapply(args:List[String]):Option[(C,List[String])] =
    try {
      Some(parse(args))
    } catch {
      case ex:UsageException if cfg.quietMode => None
    }

  private def uncamelCase(s:String) = s flatMap { c =>
    if ( c.isUpper )
      Seq('-',c.toLower)
    else
      Seq(c)
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
