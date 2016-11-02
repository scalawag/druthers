package org.scalawag.druthers

object OptionIntTest {
  case class Opts(val aopt:Option[Int],
                  val bopt:Option[Int])
}

import OptionIntTest._

class OptionIntTest extends OptionsParserTest {

  test("short - present") {
    parseOf[Opts]("-a 42 -b7",SHORT) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("short - collapse prohibited, fail") {
    fail[Opts]("-a 42 -b7",SHORT.withCollapsedValuesProhibited) match {
      case Seq(MissingValue(spec),UnknownKey("-7")) =>
        spec.key should be ("b")
    }
  }

  test("short - collapse prohibited, pass") {
    parseOf[Opts]("-a 42 -b 7",SHORT.withCollapsedValuesProhibited) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("short - collapse required, fail") {
    fail[Opts]("-a 42 -b7",SHORT.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("a")
    }
  }

  test("short - collapse required, pass") {
    parseOf[Opts]("-a42 -b7",SHORT.withCollapsedValuesRequired) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("short - absent") {
    parseOf[Opts]("-a 42",SHORT) should be ((Opts(Some(42),None),Nil))
  }

  test("short - cluster, rest is arg (not other keys)") {
    parseOf[Opts]("-a 42 -b7",SHORT.withClustering) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 42 -b x",SHORT) match {
      case Seq(InvalidValue(spec,"x",_)) => spec.key should be ("b")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 42 -bx",SHORT) match {
      case Seq(InvalidValue(spec,"x",_)) => spec.key should be ("b")
    }
  }

  test("long - present") {
    parseOf[Opts]("--aopt 42 --bopt=7",LONG) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("long - collapse prohibited, fail") {
    fail[Opts]("--aopt=42 --bopt 7",LONG.withCollapsedValuesProhibited) match {
      case Seq(UnknownKey("--aopt=42")) => // should fail like this
    }
  }

  test("long - collapse prohibited, pass") {
    parseOf[Opts]("--aopt 42 --bopt 7",LONG.withCollapsedValuesProhibited) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("long - collapse required, fail") {
    fail[Opts]("--aopt=42 --bopt 7",LONG.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("bopt")
    }
  }

  test("long - collapse required, pass") {
    parseOf[Opts]("--aopt=42 --bopt=7",LONG.withCollapsedValuesRequired) should be ((Opts(Some(42),Some(7)),Nil))
  }

  test("long - absent") {
    parseOf[Opts]("--aopt 42",LONG) should be ((Opts(Some(42),None),Nil))
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum",LONG) match {
      case Seq(InvalidValue(spec,"notanum",_)) =>
        spec.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
