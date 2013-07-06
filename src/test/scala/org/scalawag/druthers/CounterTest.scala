package org.scalawag.druthers

object CounterTest {
  case class Opts(aopt:Counter = Counter(0),
                  bopt:Counter = Counter(0))
}

import CounterTest._

class CounterTest extends OptionsParserTest {

  test("short - present") {
    parseOf[Opts]("-a",SHORT) should be ((Opts(Counter(1)),Nil))
  }

  test("short - present twice") {
    parseOf[Opts]("-a",SHORT) should be ((Opts(Counter(1))),Nil)
  }

  test("short - present with trailing bare word") {
    parseOf[Opts]("-a bare",SHORT) should be ((Opts(Counter(1)),Seq("bare")))
  }

  test("short - absent") {
    parseOf[Opts]("",SHORT) should be ((Opts(),Nil))
  }

  test("short - cluster") {
    parseOf[Opts]("-aaa",SHORT.withClustering) should be ((Opts(Counter(3)),Nil))
  }

  test("short - clustering fails with clustering disabled") {
    fail[Opts]("-aaa",SHORT) match {
      case Seq(UnexpectedValue(spec,"aa")) =>
        spec.key should be ("a")
    }
  }

  test("long - present") {
    parseOf[Opts]("--aopt",LONG) should be ((Opts(Counter(1)),Nil))
  }

  test("long - present twice") {
    parseOf[Opts]("--aopt --aopt",LONG) should be ((Opts(Counter(2)),Nil))
  }

  test("long - present with trailing bare word") {
    parseOf[Opts]("--aopt bare",LONG) should be ((Opts(Counter(1)),Seq("bare")))
  }

  test("long - absent") {
    parseOf[Opts]("",LONG) should be ((Opts(),Nil))
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=true",LONG) match {
      case Seq(UnexpectedValue(spec,"true")) =>
        spec.key should be ("aopt")
    }
  }

  test("long - specify illegal value (empty)") {
    fail[Opts]("--aopt=",LONG) match {
      case Seq(UnexpectedValue(spec,"")) =>
        spec.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
