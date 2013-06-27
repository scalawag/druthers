package org.scalawag.druthers

object SeqIntTest {
  case class Opts(val aopt:Seq[Int])
}

import SeqIntTest._

class SeqIntTest extends ParserTest {

  test("short - present") {
    succeed[Opts]("-a42 -a 7 bare",Opts(Seq(42,7)),"bare",SHORT)
  }

  test("short - collapse prohibited, fail") {
    fail[Opts]("-a 42 -a7",SHORT.withCollapsedValuesProhibited) {
      case Seq(MissingValue(flag),UnknownKey("-7")) =>
        flag.key should be ("a")
    }
  }

  test("short - collapse prohibited, pass") {
    succeed[Opts]("-a 42 -a 7",Opts(Seq(42,7)),"",SHORT.withCollapsedValuesProhibited)
  }

  test("short - collapse required, fail") {
    fail[Opts]("-a 42 -a7",SHORT.withCollapsedValuesRequired) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("a")
    }
  }

  test("short - collapse required, pass") {
    succeed[Opts]("-a42 -a7",Opts(Seq(42,7)),"",SHORT.withCollapsedValuesRequired)
  }

  test("short - absent") {
    succeed[Opts]("bare",Opts(Seq()),"bare",SHORT)
  }

  test("short - cluster, rest is arg (not other keys)") {
    succeed[Opts]("-a 42 -a7",SHORT.withClustering) { case(opts,remains) =>
      opts should be (Opts(Seq(42,7)))
      remains should be (Array.empty)
    }
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 42 -a x",SHORT) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("a")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 42 -ax",SHORT) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("a")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt 42 --aopt=7",Opts(Seq(42,7)),"",LONG)
  }

  test("long - collapse prohibited, fail") {
    fail[Opts]("--aopt=42 --aopt 7",LONG.withCollapsedValuesProhibited) {
      case Seq(UnknownKey("--aopt=42")) => // should fail like this
    }
  }

  test("long - collapse prohibited, pass") {
    succeed[Opts]("--aopt 42 --aopt 7",LONG.withCollapsedValuesProhibited) { case(opts,remains) =>
      opts should be (Opts(Seq(42,7)))
      remains should be (Array.empty)
    }
  }

  test("long - collapse required, fail") {
    fail[Opts]("--aopt=42 --aopt 7",LONG.withCollapsedValuesRequired) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("aopt")
    }
  }

  test("long - collapse required, pass") {
    succeed[Opts]("--aopt=42 --aopt=7",LONG.withCollapsedValuesRequired) { case(opts,remains) =>
      opts should be (Opts(Seq(42,7)))
      remains should be (Array.empty)
    }
  }

  test("long - absent") {
    succeed[Opts]("bare",Opts(Seq()),"bare",LONG)
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum",LONG) {
      case Seq(InvalidValue(flag,"notanum",_)) =>
        flag.key should be ("aopt")
    }
  }

  test("long - missing values") {
    fail[Opts]("--aopt",LONG) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("aopt")
    }
  }

  test("long - values separated by comma") {
    succeed[Opts]("--aopt 42,7,404",Opts(Seq(42,7,404)),"",LONG.withValueDelimiter(","))
  }

  test("long - values separated by colon") {
    succeed[Opts]("--aopt 42:7:404",Opts(Seq(42,7,404)),"",LONG.withValueDelimiter(":"))
  }

  test("long - values separated by regexp") {
    succeed[Opts]("--aopt 42delim7another404",Opts(Seq(42,7,404)),"",LONG.withValueDelimiter("[a-z]+"))
  }

  test("long - empty individual values separated by comma ignored") {
    succeed[Opts]("--aopt ,42,,,7,,404",Opts(Seq(42,7,404)),"",LONG.withValueDelimiter(","))
  }

  test("long - no non-empty individual values") {
    succeed[Opts]("--aopt ,",Opts(Seq()),"",LONG.withValueDelimiter(","))
  }

  test("long - values separated by comma fails without feature enabled") {
    fail[Opts]("--aopt 42,7,404",LONG) {
      case Seq(InvalidValue(flag,"42,7,404",_)) =>
        flag.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
