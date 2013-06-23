package org.scalawag.druthers

object IntTest {
  case class Opts(val aopt:Int,
                  val bopt:Int)
}

import IntTest._

class IntTest extends ParserTest {

  test("short - present") {
    succeed[Opts]("-a 42 -b7",ShortOptions()) { case(opts,remains) =>
      opts should be (Opts(42,7))
      remains should be (Array.empty)
    }
  }

  test("short - space must delimit value, fail") {
    fail[Opts]("-a 42 -b7",ShortOptions().withSpaceDelimitsValue(Some(true))) {
      case Seq(MissingValue(flag),UnknownKey("-7")) =>
        flag.key should be ("b")
    }
  }

  test("short - space must delimit value, pass") {
    succeed[Opts]("-a 42 -b 7",ShortOptions().withSpaceDelimitsValue(Some(true))) { case(opts,remains) =>
      opts should be (Opts(42,7))
      remains should be (Array.empty)
    }
  }

  test("short - space must not delimit value, fail") {
    fail[Opts]("-a 42 -b7",ShortOptions().withSpaceDelimitsValue(Some(false))) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("a")
    }
  }

  test("short - space must not delimit value, pass") {
    succeed[Opts]("-a42 -b7",ShortOptions().withSpaceDelimitsValue(Some(false))) { case(opts,remains) =>
      opts should be (Opts(42,7))
      remains should be (Array.empty)
    }
  }

  test("short - absent") {
    fail[Opts]("-a 42",ShortOptions()) {
      case Seq(MissingRequiredKey(flag)) => flag.key should be ("b")
    }
  }

  test("short - cluster, rest is arg (not other keys)") {
    succeed[Opts]("-a 42 -b7",ShortOptions().withClustering(true)) { case(opts,remains) =>
      opts should be (Opts(42,7))
      remains should be (Array.empty)
    }
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 42 -b x",ShortOptions()) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("b")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 42 -bx",ShortOptions()) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("b")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt 42 --bopt=7",Opts(42,7),"",LongOptions())
  }

  test("long - equals required, fail") {
    fail[Opts]("--aopt=42 --bopt 7",LongOptions().withEqualDelimitsValue(Some(true))) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("bopt")
    }
  }

  test("long - equals required, pass") {
    succeed[Opts]("--aopt=42 --bopt=7",LongOptions().withEqualDelimitsValue(Some(true))) { case(opts,remains) =>
      opts should be (Opts(42,7))
      remains should be (Array.empty)
    }
  }

  test("long - equals forbidden, fail") {
    fail[Opts]("--aopt=42 --bopt 7",LongOptions().withEqualDelimitsValue(Some(false))) {
      case Seq(UnknownKey("--aopt=42")) => // should fail like this
    }
  }

  test("long - equals must not delimit value, pass") {
    succeed[Opts]("--aopt 42 --bopt 7",LongOptions().withEqualDelimitsValue(Some(false))) { case(opts,remains) =>
      opts should be (Opts(42,7))
      remains should be (Array.empty)
    }
  }

  test("long - absent") {
    fail[Opts]("--aopt 42",LongOptions()) {
      case Seq(MissingRequiredKey(flag)) => flag.key should be ("bopt")
    }
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum",LongOptions()) {
      case Seq(InvalidValue(flag,"notanum",_)) =>
        flag.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
