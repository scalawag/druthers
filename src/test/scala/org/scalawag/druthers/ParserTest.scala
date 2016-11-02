package org.scalawag.druthers

import org.scalatest.{FunSuite, Matchers}

abstract class ParserTest extends FunSuite with Matchers {
  TestLogging // initialize test logging

  val SHORT = ParserConfiguration.withShortKeys
  val LONG = ParserConfiguration.withLongKeys

  protected def split(s:String):List[String] =
    if ( s.isEmpty )
      Nil
    else
      s.split("\\s+").toList
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
