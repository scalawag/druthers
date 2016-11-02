package org.scalawag.druthers

object QuietModeTest {
  case class Opts(a:Int)
}

import QuietModeTest._
import org.scalatest.{FunSuite, Matchers}

class QuietModeTest extends FunSuite with Matchers {
  TestLogging // initialize test logging

  private val config = ParserConfiguration.withShortKeys

  test("loud") {
    intercept[UsageException] {
      new OptionsParser[Opts](config).unapply(Nil)
    }
  }

  test("quiet") {
    new OptionsParser[Opts](config.withQuietMode).unapply(Nil) should be (None)
  }
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
