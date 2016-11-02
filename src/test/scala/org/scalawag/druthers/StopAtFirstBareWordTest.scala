package org.scalawag.druthers

object StopAtFirstBareWordTest {
  case class Opts(a:Seq[Int])
}

import StopAtFirstBareWordTest._

class StopAtFirstBareWordTest extends OptionsParserTest {
  test("don't stop") {
    parseOf[Opts]("-a 4 bare1 -a 8 bare2",SHORT) should be ((Opts(Seq(4,8)),Seq("bare1","bare2")))
  }

  test("stop") {
    parseOf[Opts]("-a 4 bare1 -a 8 bare2",SHORT.withStopAtFirstBareWord) should be ((Opts(Seq(4)),Seq("bare1","-a","8","bare2")))
  }
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
