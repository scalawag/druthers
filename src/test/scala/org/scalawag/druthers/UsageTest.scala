package org.scalawag.druthers

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

object UsageTest {
  case class Opts(@Usage("here's my documentation") a:Int,b:Int)

  class Options(@Usage("specifies the address of the server to connect to (DNS or IP)")
                val host:String,
                @Usage("specifies the port on which to connect to the server")
                val port:Int,
                @Usage("enables verbose logging")
                val verbose:Boolean,
                @Usage("the database name")
                val db:Option[String],
                @Usage("numbers to add")
                val num:Seq[Int])
}

import UsageTest._

class UsageTest extends FunSuite with ShouldMatchers {
  test("read usage from annotations") {
    val parser = new Parser[Opts](ParserConfiguration.withShortKeys)
    parser.flags(0).usage should be (Some("here's my documentation"))
    parser.flags(1).usage should be (None)
  }

  test("print long usage") {
    val parser = new Parser[Options](ParserConfiguration.withLongKeys)
    parser.usage().foreach(println)
  }

  test("print long usage (narrower)") {
    val parser = new Parser[Options](ParserConfiguration.withLongKeys)
    parser.usage(80).foreach(println)
  }

  test("print short usage") {
    val parser = new Parser[Options](ParserConfiguration.withShortKeys)
    parser.usage().foreach(println)
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
