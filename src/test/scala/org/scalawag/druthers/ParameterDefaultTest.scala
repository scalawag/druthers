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
    succeed[Opts]("",SHORT) {
      case (opts,remains) =>
        opts should be (Opts())
        remains should be (Nil)
    }
  }

  test("parameter defaults - specify string") {
    succeed[Opts]("-a other",SHORT) {
      case (opts,remains) =>
        opts should be (Opts(a = "other"))
        remains should be (Nil)
    }
  }

  test("parameter defaults - specify int") {
    succeed[Opts]("-b 16",SHORT) {
      case (opts,remains) =>
        opts should be (Opts(b = 16))
        remains should be (Nil)
    }
  }

  test("parameter defaults - specify float") {
    succeed[Opts]("-c 12",SHORT) {
      case (opts,remains) =>
        opts should be (Opts(c = 12f))
        remains should be (Nil)
    }
  }

  test("parameter defaults - specify sequence") {
    succeed[Opts]("-d o -d p",SHORT) {
      case (opts,remains) =>
        opts should be (Opts(d = Seq("o","p")))
        remains should be (Nil)
    }
  }

  test("parameter defaults - specify option") {
    succeed[Opts]("-e 15",SHORT) {
      case (opts,remains) =>
        opts should be (Opts(e = Some(15)))
        remains should be (Nil)
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
