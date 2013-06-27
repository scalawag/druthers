package org.scalawag.druthers

import scala.reflect.runtime.universe._
import scala.util.Try
import org.scalawag.timber.api.style.slf4j
import scala.util.Failure
import scala.util.Success
import java.io.PrintWriter

object Parser {
  case class Flag(key:String,
                  name:String,
                  argType:ArgType.Value,
                  cardinality:Cardinality.Value,
                  usage:Option[String])

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
}

class Parser[C:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends slf4j.Logging {
  import Parser._

  // Identifies the flags implied by the container class.

  val (flags,flagMap) = {

    val params = constructor.paramss match {
      case List(head) => head
      case _ =>
        throw new IllegalArgumentException("target class constructor takes no arguments, making it not a very useful option container")
    }

    val flags = params map { param =>
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

      Flag(key,name,argType,cardinality,usage)
    }

    val flagMap = flags.map( f => f.key -> f ).toMap

    if ( flagMap.size < flags.size ) {
      val collisions = flags.groupBy(_.key).flatMap { case (key,flags) =>
        if ( flags.length > 1 )
          Some(s"  $key => ${flags.map(_.name).mkString(" ")}")
        else
          None
      }.toSeq
      val lines = "key collisions found, use LongKeys or change your field names" +: collisions
      throw new IllegalArgumentException(lines.mkString("","\n","\n"))
    }

    (flags,flagMap)
  }

  private lazy val constructor = {

    if ( typeOf[C] <:< NOTHING_TYPE )
      throw new IllegalArgumentException(s"target class not specified, add a type parameter to Parser")

    val constructors = typeOf[C].declarations.toSeq.collect {
      case m:MethodSymbol if m.isConstructor => m
    }

    constructors match {
      case Seq(only) => only
      case seq =>
        throw new IllegalArgumentException(s"target class must have exactly one constructor, yours (${typeOf[C].typeSymbol.name}) has ${seq.length}")
    }

  }

  def parseInternal(args:Array[String]) = {
    var remainingArgs = args.toSeq
    var inOptionCluster = false
    var currentFlag:Option[Flag] = None
    var values:Map[Flag,Any] = Map.empty
    var bareWords:Seq[String] = Seq.empty
    var errors:Seq[UsageError] = Seq.empty

    def incrementValue(flag:Flag) =
      values.get(flag) match {
        case None =>
          values += ( flag -> 1 )
        case Some(existing) =>
          values += ( flag -> ( existing.asInstanceOf[Int] + 1 ) )
    }

    def addValue(flag:Flag,value:Any) {
      values.get(flag) match {
        case None => flag.cardinality match {
          case Cardinality.MULTIPLE =>
            values += ( flag -> Seq(value) )
          case Cardinality.OPTIONAL =>
            values += ( flag -> Some(value) )
          case Cardinality.REQUIRED =>
            values += ( flag -> value )
        }

        case Some(existing) => flag.cardinality match {
          case Cardinality.MULTIPLE =>
            values += ( flag -> ( existing.asInstanceOf[Seq[Any]] :+ value ) )
          case _ =>
            errors :+= DuplicateValue(flag,existing,value)
        }
      }
    }

    def addConvertedValue(flag:Flag,stringValue:String,converter:String => Any) {
      Try(converter(stringValue)) match {
        case Success(value) =>
          addValue(flag,value)
        case Failure(ex) =>
          errors :+= InvalidValue(flag,stringValue,ex.getLocalizedMessage)
      }
    }

    def addStringValue(flag:Flag,stringValue:String) {
      val converter:(String => Any) = flag.argType match {
        case ArgType.STRING => { s:String => s }
        case ArgType.INTEGER => { s:String => s.toInt }
        case ArgType.FLOAT => { s:String => s.toFloat }
        case at => throw new IllegalStateException(s"internal error: flag $flag should never use this method")
      }

      cfg.valueDelimiter match {
        case Some(delim) =>
          stringValue.split(delim).filter(_.length > 0 ).foreach { individual =>
            addConvertedValue(flag,individual,converter)
          }
        case None =>
          addConvertedValue(flag,stringValue,converter)
      }
    }

    val NoPrefixRE = "no-(.*)".r

    def getFlags(key:String):Seq[Flag] = {
      flagMap.get(key) match {
        case Some(flag) =>
          Seq(flag)
        case None =>
          if ( cfg.abbreviations ) {
            flags.filter(_.key.startsWith(key))
          } else {
            Seq()
          }
      }
    }

    // returns success
    def forFlag(key:String)(fn:Flag => Unit) = {
      finishKey
      getFlags(key) match {
        case List(flag) =>
          fn(flag)
          true
        case List() =>
          errors :+= UnknownKey(cfg.optionPrefix + key)
          false
        case all =>
          errors :+= AmbiguousKey(cfg.optionPrefix + key,all)
          false
      }
    }

    def forNoFlag(noKey:String) = {
      finishKey
      noKey match {
        case NoPrefixRE(key) if cfg.booleansNegatedByNoPrefix =>
          getFlags(key).filter(_.argType == ArgType.BOOLEAN) match {
            case List() =>
              false
            case List(flag) =>
              addValue(flag,false)
              true
            case all =>
              errors :+= AmbiguousKey(cfg.optionPrefix + noKey,all)
              true
          }
        case _ =>
          false
      }
    }

    def requiresValue(flag:Flag) = flag.argType match {
      case ArgType.BOOLEAN => false
      case ArgType.COUNTER => false
      case _ => true
    }

    def consumeKeyAndValue(flag:Flag,value:String) {
      finishKey

      flag.argType match {
        case ArgType.BOOLEAN =>
          errors :+= UnexpectedValue(flag,value)
        case ArgType.COUNTER =>
          errors :+= UnexpectedValue(flag,value)
        case _ =>
          addStringValue(flag,value)
      }
    }

    def consumeKey(flag:Flag) {
      finishKey

      flag.argType match {
        case ArgType.BOOLEAN =>
          log.debug(s"Got boolean key ${flag.key}, adding true value")
          addValue(flag,true)
        case ArgType.COUNTER =>
          log.debug(s"Got counter key ${flag.key}, incrementing value")
          incrementValue(flag)
        case _ =>
          if ( ! cfg.useLongKeys && cfg.mustCollapseValues ) {
            errors :+= MissingValue(flag)
          } else {
            log.debug(s"Got key ${flag.key}, waiting for value(s)")
            currentFlag = Some(flag)
          }
      }
    }

    def finishKey {
      // Raise an error if the last flag never got its value(s)
      currentFlag map { cf =>
        errors :+= MissingValue(cf)
      }
      // Mark that we're able to start a new flag now
      currentFlag = None
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
      currentFlag match {
        case None => remainingArgs match {

          case Seq() =>
            log.debug(s"No more tokens, returning")
            done = true

          case Seq(head,tail@_*) if head.startsWith(cfg.optionPrefix) && head.length > cfg.optionPrefix.length =>
            val bareHead = head.substring(cfg.optionPrefix.length)

            // found an arg that starts with the option prefix
            if ( cfg.useLongKeys ) {
              // try to separate it into key and value at '=' if that's possible
              if ( cfg.mayCollapseValues ) {
                bareHead.split("=",2) match {
                  // arg is separable into key and value with '='
                  case Array(key,value) =>
                    forFlag(key)(consumeKeyAndValue(_,value))
                  // arg contains no '=' and it's required
                  case Array(key) if cfg.mustCollapseValues =>
                    forFlag(key) { flag =>
                      errors :+= MissingValue(flag)
                    }
                  // arg contains no '=' but we didn't need it (it was optional)
                  case Array(key) =>
                    forNoFlag(bareHead) || forFlag(key)(consumeKey)
                }
              } else {
                // long key must appear in an arg by itself (no '=', no value)
                forNoFlag(bareHead) || forFlag(bareHead)(consumeKey)
              }
            } else { // use short keys
              // This var will keep track of the rest of the arg we still have to parse
              var rest = bareHead

              while ( ! rest.isEmpty ) {
                log.debug("REST: " + rest)
                // Look at the first character to get the (maybe first) key
                var k = rest.substring(0,1)
                rest = rest.tail

                forFlag(k) { flag =>
                  // This expression basically just says that, if there's more stuff following
                  // the key and we're allow to COLLAPSE TODO the key is expecting a value or clustering is disabled, then treat the
                  // following stuff as the value for the key.  Otherwise, consume the key
                  // and wait for its value to follow.  Note that, in some cases, this will
                  // cause an error to be raised but that's exactly what should happen in
                  // those cases.
                  if ( !rest.isEmpty && cfg.mayCollapseValues && ( requiresValue(flag) || !cfg.clustering ) ) {
                    consumeKeyAndValue(flag,rest)
                    rest = ""
                  } else {
                    consumeKey(flag)
                  }
                }

                checkForInfiniteLoop
              }
            }
            remainingArgs = tail

          case Seq(head,tail@_*) if currentFlag.isDefined =>
            // TODO: handle other cases like multiple appearances set multiple values
            addStringValue(currentFlag.get,head)
            currentFlag = None
            remainingArgs = tail

          case Seq(head,tail@_*) if inOptionCluster =>
            // First character should be treated as a option key
            forFlag(head.head.toString)(consumeKey)

          case Seq(head,tail@_*) if cfg.stopAtFirstBareWord =>
            done = true

          case Seq(head,tail@_*) =>
            bareWords :+= head
            remainingArgs = tail
        }

        case Some(flag) => remainingArgs match {
          case Seq() =>
            finishKey

          case Seq(head,tail@_*) if head.startsWith(cfg.optionPrefix) =>
            finishKey

          case Seq(head,tail@_*) =>
            log.debug(s"Collecting value $head for flag $flag")
            addStringValue(flag,head)
            currentFlag = None
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

  def parse(args:Array[String]):(C,Array[String]) = {
    val (valuesMap,remains) = parseInternal(args)

    // TODO: combine this with the one in parseInternal for one list of errors
    var errors:Seq[UsageError] = Seq.empty

    val constructorArgs = flags map { flag =>
      val value = valuesMap.get(flag)

      flag.argType match {
        case ArgType.BOOLEAN => value.getOrElse(false)
        case ArgType.COUNTER => Counter(value.getOrElse(0).asInstanceOf[Int])
        case _ => flag.cardinality match {
          case Cardinality.MULTIPLE => value.getOrElse(Seq.empty)
          case Cardinality.OPTIONAL => value.getOrElse(None)
          case Cardinality.REQUIRED =>
            value match {
              case Some(v) =>
                v
              case None =>
                errors :+= MissingRequiredKey(flag)
            }
        }
      }
    }

    log.debug { pw:PrintWriter =>
      pw.println("PARSE RESULTS:")
      ( flags zip constructorArgs) foreach { case (f,ca) =>
        pw.println(s"  $f => $ca")
      }
    }

    if ( ! errors.isEmpty )
      throw new UsageException(errors)

    val mirror = runtimeMirror(Thread.currentThread.getContextClassLoader)
    val classSymbol = typeOf[C].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val constructorMirror = classMirror.reflectConstructor(constructor)
    (constructorMirror.apply(constructorArgs:_*).asInstanceOf[C],remains.toArray)
  }

  lazy val usage = {
    flags map { flag =>
      s"  --${flag.name} <n>   ${flag.usage.getOrElse("")}"
    }
  }

  def unapply(args:Array[String]):Option[(C,Array[String])] =
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
