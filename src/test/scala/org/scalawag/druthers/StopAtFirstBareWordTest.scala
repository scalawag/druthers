package org.scalawag.druthers

object StopAtFirstBareWordTest {
  case class Opts(a:Seq[Int])
}

import StopAtFirstBareWordTest._

class StopAtFirstBareWordTest extends ParserTest {
  test("don't stop") {
    succeed[Opts]("-a 4 bare1 -a 8 bare2",Opts(Seq(4,8)),"bare1 bare2",ShortOptions())
  }

  test("stop") {
  }
  succeed[Opts]("-a 4 bare1 -a 8 bare2",Opts(Seq(4)),"bare1 -a 8 bare2",ShortOptions().withStopAtFirstBareWord())
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
