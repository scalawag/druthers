package org.scalawag.druthers

object SeqIntTest {
  case class Opts(val aopt:Seq[Int])
}

import SeqIntTest._

class SeqIntTest extends ParserTest {

  test("short - present") {
    succeed[Opts]("-a42 -a 7 bare",Opts(Seq(42,7)),"bare",ShortOptions())
  }

  test("short - space must delimit value, fail") {
    fail[Opts]("-a 42 -a7",ShortOptions().withSpaceDelimitsValue(Some(true))) {
      case Seq(MissingValue(flag),UnknownKey("-7")) =>
        flag.key should be ("a")
    }
  }

  test("short - space must delimit value, pass") {
    succeed[Opts]("-a 42 -a 7",Opts(Seq(42,7)),"",ShortOptions().withSpaceDelimitsValue(Some(true)))
  }

  test("short - space must not delimit value, fail") {
    fail[Opts]("-a 42 -a7",ShortOptions().withSpaceDelimitsValue(Some(false))) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("a")
    }
  }

  test("short - space must not delimit value, pass") {
    succeed[Opts]("-a42 -a7",Opts(Seq(42,7)),"",ShortOptions().withSpaceDelimitsValue(Some(false)))
  }

  test("short - absent") {
    succeed[Opts]("bare",Opts(Seq()),"bare",ShortOptions())
  }

  test("short - cluster, rest is arg (not other keys)") {
    succeed[Opts]("-a 42 -a7",ShortOptions().withClustering(true)) { case(opts,remains) =>
      opts should be (Opts(Seq(42,7)))
      remains should be (Array.empty)
    }
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 42 -a x",ShortOptions()) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("a")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 42 -ax",ShortOptions()) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("a")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt 42 --aopt=7",Opts(Seq(42,7)),"",LongOptions())
  }

  test("long - equals required, fail") {
    fail[Opts]("--aopt=42 --aopt 7",LongOptions().withEqualDelimitsValue(Some(true))) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("aopt")
    }
  }

  test("long - equals required, pass") {
    succeed[Opts]("--aopt=42 --aopt=7",LongOptions().withEqualDelimitsValue(Some(true))) { case(opts,remains) =>
      opts should be (Opts(Seq(42,7)))
      remains should be (Array.empty)
    }
  }

  test("long - equals forbidden, fail") {
    fail[Opts]("--aopt=42 --aopt 7",LongOptions().withEqualDelimitsValue(Some(false))) {
      case Seq(UnknownKey("--aopt=42")) => // should fail like this
    }
  }

  test("long - equals must not delimit value, pass") {
    succeed[Opts]("--aopt 42 --aopt 7",LongOptions().withEqualDelimitsValue(Some(false))) { case(opts,remains) =>
      opts should be (Opts(Seq(42,7)))
      remains should be (Array.empty)
    }
  }

  test("long - absent") {
    succeed[Opts]("bare",Opts(Seq()),"bare",LongOptions())
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum",LongOptions()) {
      case Seq(InvalidValue(flag,"notanum",_)) =>
        flag.key should be ("aopt")
    }
  }

  test("long - missing values") {
    fail[Opts]("--aopt",LongOptions()) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("aopt")
    }
  }

  test("long - values separated by comma") {
    succeed[Opts]("--aopt 42,7,404",Opts(Seq(42,7,404)),"",LongOptions().withMultipleValueDelimiter(","))
  }

  test("long - values separated by colon") {
    succeed[Opts]("--aopt 42:7:404",Opts(Seq(42,7,404)),"",LongOptions().withMultipleValueDelimiter(":"))
  }

  test("long - values separated by regexp") {
    succeed[Opts]("--aopt 42delim7another404",Opts(Seq(42,7,404)),"",LongOptions().withMultipleValueDelimiter("[a-z]+"))
  }

  test("long - empty individual values separated by comma ignored") {
    succeed[Opts]("--aopt ,42,,,7,,404",Opts(Seq(42,7,404)),"",LongOptions().withMultipleValueDelimiter(","))
  }

  test("long - no non-empty individual values") {
    succeed[Opts]("--aopt ,",Opts(Seq()),"",LongOptions().withMultipleValueDelimiter(","))
  }

  test("long - values separated by comma fails without feature enabled") {
    fail[Opts]("--aopt 42,7,404",LongOptions()) {
      case Seq(InvalidValue(flag,"42,7,404",_)) =>
        flag.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
