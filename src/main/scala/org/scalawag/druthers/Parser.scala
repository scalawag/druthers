package org.scalawag.druthers

import scala.reflect.runtime.universe._

object Parser {

  object Cardinality extends Enumeration {
    val OPTIONAL = Value
    val REQUIRED = Value
    val MULTIPLE = Value
  }

  val STRING_TYPE = typeOf[String]
  val INTEGER_TYPE = typeOf[Int]
  val FLOAT_TYPE = typeOf[Float]
  val BOOLEAN_TYPE = typeOf[Boolean]
  val COUNTER_TYPE = typeOf[Counter]

  private val SEQUENCE_TYPE = typeOf[Seq[Any]]
  private val OPTION_TYPE = typeOf[Option[Any]]

  private val USAGE_TYPE = typeOf[Usage]
  private val VALUE_TERM = newTermName("value")

  private val NOTHING_TYPE = typeOf[Nothing]
}

import Parser._

class Parser[C:TypeTag](classLoader:Option[ClassLoader] = None) {

  protected[this] val constructor = {

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

  protected[this] val mirror = runtimeMirror(classLoader.getOrElse(Thread.currentThread.getContextClassLoader))

  protected[this] val constructorMirror = {
    val classSymbol = typeOf[C].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    classMirror.reflectConstructor(constructor)
  }

  protected[this] def getParams =
    constructor.paramss match {
      case Seq(head) => head
      case _ =>
        throw new IllegalArgumentException("target class constructor takes no arguments, making it not a very useful option container")
    }

  protected[this] def getParameterDefaults() = {
    val cls = typeOf[C].typeSymbol.asClass
    cls.companionSymbol match {
      case NoSymbol =>
        // No companion object means no default parameters
        constructor.paramss.head.map( _ => None )

      case mdl:Symbol =>
        val im = mirror.reflect((mirror.reflectModule(mdl.asModule)).instance)
        val ts = im.symbol.typeSignature

        constructor.paramss.head.zipWithIndex.map { case (p,n) =>
          val name = newTermName(s"$$lessinit$$greater$$default$$${n+1}")
          val defarg = ts.member(name)
          if (defarg == NoSymbol) {
            None
          } else {
            Some((im reflectMethod defarg.asMethod)())
          }
        }
    }
  }

  protected[this] def getCardinalityAndValueType(tpe:Type) =
    if ( tpe.erasure =:= SEQUENCE_TYPE )
      (Cardinality.MULTIPLE,tpe.asInstanceOf[TypeRef].args.head)
    else if ( tpe.erasure =:= OPTION_TYPE )
      (Cardinality.OPTIONAL,tpe.asInstanceOf[TypeRef].args.head)
    else
      (Cardinality.REQUIRED,tpe)

  protected[this] def getUsage(symbol:Symbol) =
    symbol.annotations.find(_.tpe =:= USAGE_TYPE).flatMap(_.javaArgs.get(VALUE_TERM)).map {
      case LiteralArgument(Constant(s:String)) => s
    }

  protected[this] def instantiate(args:List[Any]) = {
    constructorMirror.apply(args:_*).asInstanceOf[C]
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
