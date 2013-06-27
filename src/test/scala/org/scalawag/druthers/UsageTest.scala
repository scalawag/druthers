package org.scalawag.druthers

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

object UsageTest {
  case class Opts(@Usage("here's my documentation") a:Int,b:Int)
}

import UsageTest._

class UsageTest extends FunSuite with ShouldMatchers {
  test("basic usage") {
    val parser = new Parser[Opts](ParserConfiguration.withShortKeys)
    parser.flags(0).usage should be (Some("here's my documentation"))
    parser.flags(1).usage should be (None)
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
