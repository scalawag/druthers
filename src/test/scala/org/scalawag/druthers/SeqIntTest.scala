package org.scalawag.druthers

object SeqIntTest {
  case class Opts(val aopt:Seq[Int])
}

import SeqIntTest._

class SeqIntTest extends OptionsParserTest {

  test("short - present") {
    parseOf[Opts]("-a42 -a 7 bare",SHORT) should be ((Opts(Seq(42,7)),Seq("bare")))
  }

  test("short - collapse prohibited, fail") {
    fail[Opts]("-a 42 -a7",SHORT.withCollapsedValuesProhibited) match {
      case Seq(MissingValue(spec),UnknownKey("-7")) =>
        spec.key should be ("a")
    }
  }

  test("short - collapse prohibited, pass") {
    parseOf[Opts]("-a 42 -a 7",SHORT.withCollapsedValuesProhibited) should be ((Opts(Seq(42,7)),Nil))
  }

  test("short - collapse required, fail") {
    fail[Opts]("-a 42 -a7",SHORT.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("a")
    }
  }

  test("short - collapse required, pass") {
    parseOf[Opts]("-a42 -a7",SHORT.withCollapsedValuesRequired) should be ((Opts(Seq(42,7)),Nil))
  }

  test("short - absent") {
    parseOf[Opts]("bare",SHORT) should be ((Opts(Seq()),Seq("bare")))
  }

  test("short - cluster, rest is arg (not other keys)") {
    parseOf[Opts]("-a 42 -a7",SHORT.withClustering) should be ((Opts(Seq(42,7)),Nil))
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 42 -a x",SHORT) match {
      case Seq(InvalidValue(spec,"x",_)) => spec.key should be ("a")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 42 -ax",SHORT) match {
      case Seq(InvalidValue(spec,"x",_)) => spec.key should be ("a")
    }
  }

  test("long - present") {
    parseOf[Opts]("--aopt 42 --aopt=7",LONG) should be ((Opts(Seq(42,7)),Nil))
  }

  test("long - collapse prohibited, fail") {
    fail[Opts]("--aopt=42 --aopt 7",LONG.withCollapsedValuesProhibited) should be (Seq(UnknownKey("--aopt=42")))
  }

  test("long - collapse prohibited, pass") {
    parseOf[Opts]("--aopt 42 --aopt 7",LONG.withCollapsedValuesProhibited) should be ((Opts(Seq(42,7)),Nil))
  }

  test("long - collapse required, fail") {
    fail[Opts]("--aopt=42 --aopt 7",LONG.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("aopt")
    }
  }

  test("long - collapse required, pass") {
    parseOf[Opts]("--aopt=42 --aopt=7",LONG.withCollapsedValuesRequired) should be ((Opts(Seq(42,7)),Nil))
  }

  test("long - absent") {
    parseOf[Opts]("bare",LONG) should be ((Opts(Seq()),Seq("bare")))
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum",LONG) match {
      case Seq(InvalidValue(spec,"notanum",_)) =>
        spec.key should be ("aopt")
    }
  }

  test("long - missing values") {
    fail[Opts]("--aopt",LONG) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("aopt")
    }
  }

  test("long - values separated by comma") {
    parseOf[Opts]("--aopt 42,7,404",LONG.withValueDelimiter(",")) should be ((Opts(Seq(42,7,404)),Nil))
  }

  test("long - values separated by colon") {
    parseOf[Opts]("--aopt 42:7:404",LONG.withValueDelimiter(":")) should be ((Opts(Seq(42,7,404)),Nil))
  }

  test("long - values separated by regexp") {
    parseOf[Opts]("--aopt 42delim7another404",LONG.withValueDelimiter("[a-z]+")) should be ((Opts(Seq(42,7,404)),Nil))
  }

  test("long - empty individual values separated by comma ignored") {
    parseOf[Opts]("--aopt ,42,,,7,,404",LONG.withValueDelimiter(",")) should be ((Opts(Seq(42,7,404)),Nil))
  }

  test("long - no non-empty individual values") {
    parseOf[Opts]("--aopt ,",LONG.withValueDelimiter(",")) should be ((Opts(Seq()),Nil))
  }

  test("long - values separated by comma fails without feature enabled") {
    fail[Opts]("--aopt 42,7,404",LONG) match {
      case Seq(InvalidValue(spec,"42,7,404",_)) =>
        spec.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
