package org.scalawag.druthers

object FloatTest {
  case class Opts(val aopt:Float,
                  val bopt:Float)
}

import FloatTest._

class FloatTest extends ParserTest {

  test("short - present") {
    succeed[Opts]("-a 4.2 -b7.1",SHORT) { case(opts,remains) =>
      opts should be (Opts(4.2f,7.1f))
      remains should be (Array.empty)
    }
  }

  test("short - collapse prohibited, fail") {
    fail[Opts]("-a 4.2 -b7.1",SHORT.withCollapsedValuesProhibited) {
      case Seq(MissingValue(flag),UnknownKey("-7"),UnknownKey("-."),UnknownKey("-1")) =>
        flag.key should be ("b")
    }
  }

  test("short - collapse prohibited, pass") {
    succeed[Opts]("-a 4.2 -b 7.1",SHORT.withCollapsedValuesProhibited) { case(opts,remains) =>
      opts should be (Opts(4.2f,7.1f))
      remains should be (Array.empty)
    }
  }

  test("short - collapse required, fail") {
    fail[Opts]("-a 4.2 -b7.1",SHORT.withCollapsedValuesRequired) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("a")
    }
  }

  test("short - collapse required, pass") {
    succeed[Opts]("-a4.2 -b7.1",SHORT.withCollapsedValuesRequired) { case(opts,remains) =>
      opts should be (Opts(4.2f,7.1f))
      remains should be (Array.empty)
    }
  }

  test("short - absent") {
    fail[Opts]("-a 4.2",SHORT) {
      case Seq(MissingRequiredKey(flag)) => flag.key should be ("b")
    }
  }

  test("short - cluster, rest is arg (not other keys)") {
    succeed[Opts]("-a 4.2 -b7.1",SHORT.withClustering) { case(opts,remains) =>
      opts should be (Opts(4.2f,7.1f))
      remains should be (Array.empty)
    }
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 4.2 -b x",SHORT) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("b")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 4.2 -bx",SHORT) {
      case Seq(InvalidValue(flag,"x",_)) => flag.key should be ("b")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt 4.2 --bopt=7.1",Opts(4.2f,7.1f),"",LONG)
  }

  test("long - collapse prohibited, fail") {
    fail[Opts]("--aopt=4.2 --bopt 7.1",LONG.withCollapsedValuesProhibited) {
      case Seq(UnknownKey("--aopt=4.2")) => // should fail like this
    }
  }

  test("long - collapse prohibited, pass") {
    succeed[Opts]("--aopt 4.2 --bopt 7.1",LONG.withCollapsedValuesProhibited) { case(opts,remains) =>
      opts should be (Opts(4.2f,7.1f))
      remains should be (Array.empty)
    }
  }

  test("long - collapse required, fail") {
    fail[Opts]("--aopt=4.2 --bopt 7.1",LONG.withCollapsedValuesRequired) {
      case Seq(MissingValue(flag)) =>
        flag.key should be ("bopt")
    }
  }

  test("long - collapse required, pass") {
    succeed[Opts]("--aopt=4.2 --bopt=7.1",LONG.withCollapsedValuesRequired) { case(opts,remains) =>
      opts should be (Opts(4.2f,7.1f))
      remains should be (Array.empty)
    }
  }

  test("long - absent") {
    fail[Opts]("--aopt 4.2",LONG) {
      case Seq(MissingRequiredKey(flag)) => flag.key should be ("bopt")
    }
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum",LONG) {
      case Seq(InvalidValue(flag,"notanum",_)) =>
        flag.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
