package org.scalawag.druthers

import scala.reflect.runtime.universe._

object Parser {

  object Cardinality extends Enumeration {
    val OPTIONAL = Value
    val REQUIRED = Value
    val MULTIPLE = Value
  }

  private val USAGE_TYPE = typeOf[Usage]
  private val VALUE_TERM = newTermName("value")

  private val NOTHING_TYPE = typeOf[Nothing]
}

import Parser._

class Parser[C:TypeTag] {

  protected[this] lazy val constructor = {

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

  protected[this] def getUsage(symbol:Symbol) =
    symbol.annotations.find(_.tpe =:= USAGE_TYPE).flatMap(_.javaArgs.get(VALUE_TERM)).map {
      case LiteralArgument(Constant(s:String)) => s
    }

}
