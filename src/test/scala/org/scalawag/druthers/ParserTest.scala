package org.scalawag.druthers

import scala.reflect.runtime.universe.TypeTag
import org.scalawag.druthers.impl.Parser
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

abstract class ParserTest extends FunSuite with ShouldMatchers {
  def succeed[T:TypeTag](args:String,opts:T,remains:String,config:ParserConfiguration):Unit =
    succeed(args,opts,remains,new Parser[T](config))

  def succeed[T:TypeTag](args:String,opts:T,remains:String,parser:Parser[T]) {
    val (o,r) = parser.parse(array(args))

    o should be (opts)
    r should be (array(remains))
  }

  def succeed[T:TypeTag](args:String,config:ParserConfiguration)(fn:PartialFunction[(T,Array[String]),Unit]) {
    val parser = new Parser[T](config)
    val (o,r) = parser.parse(array(args))
    fn((o,r))
  }

  def fail[T:TypeTag](args:String,config:ParserConfiguration)(fn:PartialFunction[Seq[UsageError],Unit]) {
    fn(intercept[UsageException]((new Parser[T](config)).parse(array(args))).errors)
  }

  private def array(s:String):Array[String] =
    if ( s.isEmpty )
      Array.empty
    else
      s.split("\\s+")
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
