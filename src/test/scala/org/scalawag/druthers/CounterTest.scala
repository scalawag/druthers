package org.scalawag.druthers

object CounterTest {
  case class Opts(aopt:Counter = Counter(0),
                  bopt:Counter = Counter(0))
}

import CounterTest._

class CounterTest extends ParserTest {

  test("short - present") {
    succeed[Opts]("-a",ShortOptions()) { case(opts,remains) =>
      opts should be (Opts(Counter(1)))
      remains should be (Array.empty)
    }
  }

  test("short - present twice") {
    succeed[Opts]("-a",ShortOptions()) { case(opts,remains) =>
      opts should be (Opts(Counter(1)))
      remains should be (Array.empty)
    }
  }

  test("short - present with trailing bare word") {
    succeed[Opts]("-a bare",Opts(Counter(1)),"bare",ShortOptions())
  }

  test("short - absent") {
    succeed[Opts]("",Opts(),"",ShortOptions())
  }

  test("short - cluster") {
    succeed[Opts]("-aaa",Opts(Counter(3)),"",ShortOptions().withClustering(true))
  }

  test("short - clustering fails with clustering disabled") {
    fail[Opts]("-aaa",ShortOptions()) {
      case Seq(UnexpectedValue(flag,"aa")) =>
        flag.key should be ("a")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt",Opts(Counter(1)),"",LongOptions())
  }

  test("long - present twice") {
    succeed[Opts]("--aopt --aopt",LongOptions()) { case(opts,remains) =>
      opts should be (Opts(Counter(2)))
      remains should be (Array.empty)
    }
  }

  test("long - present with trailing bare word") {
    succeed[Opts]("--aopt bare",Opts(Counter(1)),"bare",LongOptions())
  }

  test("long - absent") {
    succeed[Opts]("",Opts(),"",LongOptions())
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=true",LongOptions()) {
      case Seq(UnexpectedValue(flag,"true")) =>
        flag.key should be ("aopt")
    }
  }

  test("long - specify illegal value (empty)") {
    fail[Opts]("--aopt=",LongOptions()) {
      case Seq(UnexpectedValue(flag,"")) =>
        flag.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
