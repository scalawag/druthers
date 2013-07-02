package org.scalawag.druthers

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import org.scalawag.timber.api.style.slf4j
import scala.util.{Try, Success, Failure}
import java.io.PrintWriter
import org.scalawag.druthers.Parser._
import org.scalawag.druthers.CommandParser.ArgumentSpec.Type
import scala.annotation.tailrec

object CommandParser {
  case class Options[C](name:String,parser:OptionsParser[C])

  object ArgumentSpec {
    object Type extends Enumeration {
      val STRING = Value
      val INTEGER = Value
      val FLOAT = Value
      val BOOLEAN = Value
      val OPTIONS = Value
    }
  }

  case class ArgumentSpec(name:String,
                          argType:ArgumentSpec.Type.Value,
                          cardinality:Cardinality.Value,
                          usage:Option[String])
}

class CommandParser[C:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends Parser[C] with slf4j.Logging {
  import CommandParser._

  def createParser(tpe:universe.Type,cfg:ParserConfiguration):Option[OptionsParser[_]] = None

  val arguments = {

    @tailrec
    def process(ins:List[Symbol],seenOptions:Boolean = false,outs:List[Any] = Nil):List[Any] =  ins match {

      case Nil => outs

      case head :: tail =>
        val name = head.name.toString
        val typeSignature = head.typeSignatureIn(typeOf[C])

        val usage = getUsage(head)

        val (cardinality,valueTypeSignature) = getCardinalityAndValueType(typeSignature)

        val valueType =
          if ( valueTypeSignature =:= STRING_TYPE )
            Type.STRING
          else if ( valueTypeSignature =:= INTEGER_TYPE )
            Type.INTEGER
          else if ( valueTypeSignature =:= FLOAT_TYPE )
            Type.FLOAT
          else if ( valueTypeSignature =:= BOOLEAN_TYPE )
            Type.BOOLEAN
          else
            Type.OPTIONS

        val out =
          if ( valueType == Type.OPTIONS ) {
            val effectiveConfig =
              if ( seenOptions )
                cfg match {
                  case l:LongOptions => l.withStopAtFirstBareWord
                  case s:ShortOptions => s.withStopAtFirstBareWord
                }
              else
                cfg

            createParser(typeSignature,effectiveConfig).map(Options(name,_)) getOrElse {
              throw new IllegalArgumentException(s"couldn't find a OptionsParser for parameter type for argument '$name': ${typeSignature}")
            }
          } else {
            ArgumentSpec(name,valueType,cardinality,usage)
          }

        process(tail,seenOptions || valueType == Type.OPTIONS,out :: outs)
    }

    // The process function works backwards so that it can detect the last Options argument and treat it
    // specially (by allowing it to keep stopAtFirstBareWord off).  Because of that, we need to reverse the
    // parameters before we pass them in to be processed.

    val params = getParams(constructor)

    process(params.reverse)
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

        case ( spec@ArgumentSpec(name,argType,cardinality,_) ) :: _ =>

          val conversion = args match {
            case head :: _ =>
              val c = argType match {
                case Type.STRING => Try(head)
                case Type.INTEGER => Try(head.toInt)
                case Type.FLOAT => Try(head.toFloat)
                case Type.BOOLEAN => Try(head.toBoolean)
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

    instantiate(constructorArgs)
  }

  def unapply(args:Array[String]):Option[C] = unapply(args.toList)

  def unapply(args:List[String]):Option[C] =
    try {
      Some(parse(args))
    } catch {
      case ex:UsageException if cfg.quietMode => None
    }
}

class CommandParser1[C:TypeTag,P1:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends CommandParser[C] with slf4j.Logging {
  override def createParser(tpe:universe.Type,cfg:ParserConfiguration):Option[OptionsParser[_]] = {
    if ( tpe =:= typeOf[P1] )
      Some(new OptionsParser[P1](cfg))
    else
      None
  }
}

class CommandParser2[C:TypeTag,P1:TypeTag,P2:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends CommandParser[C] with slf4j.Logging {
  override def createParser(tpe:universe.Type,cfg:ParserConfiguration):Option[OptionsParser[_]] = {
    if ( tpe =:= typeOf[P1] )
      Some(new OptionsParser[P1](cfg))
    else if ( tpe =:= typeOf[P2] )
      Some(new OptionsParser[P2](cfg))
    else
      None
  }
}

class CommandParser3[C:TypeTag,P1:TypeTag,P2:TypeTag,P3:TypeTag](cfg:ParserConfiguration = ShortOptions()) extends CommandParser[C] with slf4j.Logging {
  override def createParser(tpe:universe.Type,cfg:ParserConfiguration):Option[OptionsParser[_]] = {
    if ( tpe =:= typeOf[P1] )
      Some(new OptionsParser[P1](cfg))
    else if ( tpe =:= typeOf[P2] )
      Some(new OptionsParser[P2](cfg))
    else if ( tpe =:= typeOf[P3] )
      Some(new OptionsParser[P3](cfg))
    else
      None
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
