package org.scalawag.druthers

import scala.reflect.runtime.universe._
import org.scalawag.timber.api.style.slf4j
import scala.util.{Try, Success, Failure}
import java.io.PrintWriter

object CommandParser {
  case class Options[C](name:String,parser:OptionsParser[C])

  case class Argument(name:String,
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
    val OPTIONS = Value
  }

  private val SEQUENCE_TYPE = typeOf[Seq[Any]]
  private val OPTION_TYPE = typeOf[Option[Any]]
  private val STRING_TYPE = typeOf[String]
  private val INTEGER_TYPE = typeOf[Int]
  private val FLOAT_TYPE = typeOf[Float]
  private val BOOLEAN_TYPE = typeOf[Boolean]

  private val USAGE_TYPE = typeOf[Usage]
  private val VALUE_TERM = newTermName("value")

  private val NOTHING_TYPE = typeOf[Nothing]
}

class CommandParser[C:TypeTag,P1:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends slf4j.Logging {
  import CommandParser._

  val arguments = {

    val params = constructor.paramss match {
      case Seq(head) => head
      case _ =>
        throw new IllegalArgumentException("target class constructor takes no arguments, making it not a very useful option container")
    }

    params map { param =>
      val name = param.name.toString
      val typeSignature = param.typeSignatureIn(typeOf[C])

      val usage = param.annotations.find(_.tpe =:= USAGE_TYPE).flatMap(_.javaArgs.get(VALUE_TERM)).map {
        case LiteralArgument(Constant(s:String)) => s
      }

      val (cardinality,argTypeSignature) =
        if ( typeSignature.erasure =:= SEQUENCE_TYPE )
          (Cardinality.MULTIPLE,typeSignature.asInstanceOf[TypeRef].args.head)
        else if ( typeSignature.erasure =:= OPTION_TYPE )
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
        else if ( argTypeSignature <:< BOOLEAN_TYPE )
          ArgType.BOOLEAN
        else
          ArgType.OPTIONS

      if ( argType == ArgType.OPTIONS ) {
        if ( param.typeSignature =:= typeOf[P1] )
          Options(name,new OptionsParser[P1](cfg))
        else
          throw new IllegalArgumentException(s"couldn't find a OptionsParser for parameter type for argument '$name': ${param.typeSignature}")
      } else {
        Argument(name,argType,cardinality,usage)
      }
    }
  }

  private lazy val constructor = {

    if ( typeOf[C] <:< NOTHING_TYPE )
      throw new IllegalArgumentException(s"target class not specified, add a type parameter to CommandParser")

    val constructors = typeOf[C].declarations.collect {
      case m:MethodSymbol if m.isConstructor => m
    }.toSeq

    constructors match {
      case Seq(only) => only
      case seq =>
        throw new IllegalArgumentException(s"target class must have exactly one constructor, yours (${typeOf[C].typeSymbol.name}) has ${seq.length}")
    }

  }

  def parse(args:Array[String]):C = parse(args.toList)

  def parse(args:List[String]):C = {

    def parseHelper(args:List[String],specs:List[Any],constructorArgs:List[Option[Any]]):Try[List[Option[Any]]] = {
      log.debug(s"PARSE: args=$args specs=$specs constructorArgs=$constructorArgs")

      specs match {

        case Nil =>
          if ( args.isEmpty )
            Success(constructorArgs)
          else
            Failure(UsageException(List(ExtraneousValues(args))))

        case ( spec@Argument(name,argType,cardinality,_) ) :: _ =>

          val conversion = args match {
            case head :: _ =>
              val c = argType match {
                case ArgType.STRING => Try(head)
                case ArgType.INTEGER => Try(head.toInt)
                case ArgType.FLOAT => Try(head.toFloat)
                case ArgType.BOOLEAN => Try(head.toBoolean)
              }

              c.recoverWith {
                case ex => Failure(UsageException(List(InvalidArgument(spec,head,ex.toString))))
              }

            case Nil =>
              Failure(UsageException(List(MissingArgument(spec))))
          }

          // Called to indicate that the first spec chooses not to consume the first arg.
          // transform is the method used to modify the head of the constructorArgs.

          def pass(empty:Any) = {
            val existingSomeOrElseEmptySome = Seq(constructorArgs.head,Some(empty)).flatten.headOption
            parseHelper(args,specs.tail,None :: existingSomeOrElseEmptySome :: constructorArgs.tail)
          }

          // Call to indicate that the first spec chooses to consume the first arg.
          // transform is the method used to modify the head of the constructorArgs.
          // This leaves itself in the first spec position, meaning it may consume more.

          def consume(transform:PartialFunction[Option[Any],Option[Any]]) = {
            parseHelper(args.tail,specs,transform(constructorArgs.head) :: constructorArgs.tail)
          }

          // Call to indicate that the first spec chooses to consume the first arg.
          // transform is the method used to modify the head of the constructorArgs.
          // This relinquishes control to the next spec in the list, meaning the spec can consume no more.

          def consumeAndSate(transform:PartialFunction[Option[Any],Option[Any]]) = {
            parseHelper(args.tail,specs.tail,None :: transform(constructorArgs.head) :: constructorArgs.tail)
          }

          conversion flatMap { value =>
            cardinality match {

              case Cardinality.MULTIPLE =>
                consume {
                  case None => Some(List(value))
                  case Some(values:List[Any]) => Some(values :+ value)
                }

              case Cardinality.OPTIONAL =>
                consumeAndSate {
                  case None => Some(value)
                }

              case Cardinality.REQUIRED =>
                consumeAndSate {
                  case None => Some(value)
                }

            }
          } recoverWith { case thrown =>
            cardinality match {
              case Cardinality.MULTIPLE =>
                pass(Nil)
              case Cardinality.OPTIONAL =>
                pass(None)
              case Cardinality.REQUIRED =>
                // We can't pass because this is a required argument.  We know that there's no value here
                // because we would have sated the required spec when we saw it (above).  Ergo, just fail.
                Failure(thrown)
            }
          }

        case Options(name,parser) :: _ =>
          Try {
            parser.parse(args)
          } flatMap { case (opts,remains) =>
            parseHelper(remains,specs.tail,None :: Some(opts) :: constructorArgs.tail)
          }

      }
    }

    // flatten to drop the leading None and unwrap the Some's
    // reverse to reflect the real order of the arguments
    // throw the UsageException if that's what we got
    val constructorArgs = parseHelper(args,arguments,List(None)).get.flatten.reverse

    log.debug { pw:PrintWriter =>
      pw.println("COMMAND PARSE RESULTS:")
      ( arguments zip constructorArgs) foreach { case (f,ca) =>
        pw.println(s"  $f => $ca")
      }
    }

    val mirror = runtimeMirror(Thread.currentThread.getContextClassLoader)
    val classSymbol = typeOf[C].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val constructorMirror = classMirror.reflectConstructor(constructor)
    constructorMirror.apply(constructorArgs:_*).asInstanceOf[C]
  }

  def unapply(args:Array[String]):Option[C] = unapply(args.toList)

  def unapply(args:List[String]):Option[C] =
    try {
      Some(parse(args))
    } catch {
      case ex:UsageException if cfg.quietMode => None
    }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
