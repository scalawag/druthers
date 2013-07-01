package org.scalawag.druthers

object CounterTest {
  case class Opts(aopt:Counter = Counter(0),
                  bopt:Counter = Counter(0))
}

import CounterTest._

class CounterTest extends OptionsParserTest {

  test("short - present") {
    succeed[Opts]("-a",SHORT) { case(opts,remains) =>
      opts should be (Opts(Counter(1)))
      remains should be (Nil)
    }
  }

  test("short - present twice") {
    succeed[Opts]("-a",SHORT) { case(opts,remains) =>
      opts should be (Opts(Counter(1)))
      remains should be (Nil)
    }
  }

  test("short - present with trailing bare word") {
    succeed[Opts]("-a bare",Opts(Counter(1)),"bare",SHORT)
  }

  test("short - absent") {
    succeed[Opts]("",Opts(),"",SHORT)
  }

  test("short - cluster") {
    succeed[Opts]("-aaa",Opts(Counter(3)),"",SHORT.withClustering)
  }

  test("short - clustering fails with clustering disabled") {
    fail[Opts]("-aaa",SHORT) {
      case Seq(UnexpectedValue(spec,"aa")) =>
        spec.key should be ("a")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt",Opts(Counter(1)),"",LONG)
  }

  test("long - present twice") {
    succeed[Opts]("--aopt --aopt",LONG) { case(opts,remains) =>
      opts should be (Opts(Counter(2)))
      remains should be (Nil)
    }
  }

  test("long - present with trailing bare word") {
    succeed[Opts]("--aopt bare",Opts(Counter(1)),"bare",LONG)
  }

  test("long - absent") {
    succeed[Opts]("",Opts(),"",LONG)
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=true",LONG) {
      case Seq(UnexpectedValue(spec,"true")) =>
        spec.key should be ("aopt")
    }
  }

  test("long - specify illegal value (empty)") {
    fail[Opts]("--aopt=",LONG) {
      case Seq(UnexpectedValue(spec,"")) =>
        spec.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
