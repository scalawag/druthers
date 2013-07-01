package org.scalawag.druthers

import scala.reflect.runtime.universe.TypeTag
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

abstract class OptionsParserTest extends FunSuite with ShouldMatchers {
  val SHORT = ParserConfiguration.withShortKeys
  val LONG = ParserConfiguration.withLongKeys

  def succeed[T:TypeTag](args:String,opts:T,remains:String,config:ParserConfiguration):Unit =
    succeed(args,opts,remains,new OptionsParser[T](config))

  def succeed[T:TypeTag](args:String,opts:T,remains:String,parser:OptionsParser[T]) {
    val (o,r) = parser.parse(split(args))

    o should be (opts)
    r should be (split(remains))
  }

  def succeed[T:TypeTag](args:String,config:ParserConfiguration)(fn:PartialFunction[(T,List[String]),Unit]) {
    val parser = new OptionsParser[T](config)
    val (o,r) = parser.parse(split(args))
    fn((o,r))
  }

  def fail[T:TypeTag](args:String,config:ParserConfiguration)(fn:PartialFunction[Seq[UsageError],Unit]) {
    fn(intercept[UsageException]((new OptionsParser[T](config)).parse(split(args))).errors)
  }

  protected def split(s:String):List[String] =
    if ( s.isEmpty )
      Nil
    else
      s.split("\\s+").toList
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
