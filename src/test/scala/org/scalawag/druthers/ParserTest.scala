package org.scalawag.druthers

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

abstract class ParserTest extends FunSuite with ShouldMatchers {
  val SHORT = ParserConfiguration.withShortKeys
  val LONG = ParserConfiguration.withLongKeys

  protected def split(s:String):List[String] =
    if ( s.isEmpty )
      Nil
    else
      s.split("\\s+").toList
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
