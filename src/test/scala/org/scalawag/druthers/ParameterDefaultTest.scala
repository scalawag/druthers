package org.scalawag.druthers

object ParameterDefaultTest {
  case class Opts(val a:String = "name",
                  val b:Int = 8,
                  val c:Float = 4f,
                  val d:Seq[String] = Seq("x","y","z"),
                  val e:Option[Int] = Some(42))
}

import ParameterDefaultTest._

class ParameterDefaultTest extends OptionsParserTest {

  test("parameter defaults") {
    parseOf[Opts]("",SHORT) should be ((Opts(),Nil))
  }

  test("parameter defaults - specify string") {
    parseOf[Opts]("-a other",SHORT) should be ((Opts(a = "other"),Nil))
  }

  test("parameter defaults - specify int") {
    parseOf[Opts]("-b 16",SHORT) should be ((Opts(b = 16),Nil))
  }

  test("parameter defaults - specify float") {
    parseOf[Opts]("-c 12",SHORT) should be ((Opts(c = 12f),Nil))
  }

  test("parameter defaults - specify sequence") {
    parseOf[Opts]("-d o -d p",SHORT) should be ((Opts(d = Seq("o","p")),Nil))
  }

  test("parameter defaults - specify option") {
    parseOf[Opts]("-e 15",SHORT) should be ((Opts(e = Some(15)),Nil))
  }
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
