package org.scalawag.druthers

object IntTest {
  case class Opts(val aopt:Int,
                  val bopt:Int)
}

import IntTest._

class IntTest extends OptionsParserTest {

  test("short - present") {
    parseOf[Opts]("-a 42 -b7",SHORT) should be ((Opts(42,7),Nil))
  }

  test("short - collapse prohibited, fail") {
    fail[Opts]("-a 42 -b7",SHORT.withCollapsedValuesProhibited) match {
      case Seq(MissingValue(spec),UnknownKey("-7")) =>
        spec.key should be ("b")
    }
  }

  test("short - collapse prohibited, pass") {
    parseOf[Opts]("-a 42 -b 7",SHORT.withCollapsedValuesProhibited) should be ((Opts(42,7),Nil))
  }

  test("short - collapse required, fail") {
    fail[Opts]("-a 42 -b7",SHORT.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("a")
    }
  }

  test("short - collapse required, pass") {
    parseOf[Opts]("-a42 -b7",SHORT.withCollapsedValuesRequired) should be ((Opts(42,7),Nil))
  }

  test("short - absent") {
    fail[Opts]("-a 42",SHORT) match {
      case Seq(MissingRequiredKey(spec)) => spec.key should be ("b")
    }
  }

  test("short - cluster, rest is arg (not other keys)") {
    parseOf[Opts]("-a 42 -b7",SHORT.withClustering) should be ((Opts(42,7),Nil))
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

  test("short - require arg that is itself an option") {
    fail[Opts]("-a -b 7",SHORT) match {
      case Seq(MissingValue(spec)) => spec.key should be ("a")
    }
  }

  test("short - duplicate value failure") {
    fail[Opts]("-a 42 -b 7 -a 8",SHORT) match {
      case Seq(DuplicateValue(spec,42,8)) => spec.key should be ("a")
    }
  }

  test("long - present") {
    parseOf[Opts]("--aopt 42 --bopt=7",LONG) should be ((Opts(42,7),Nil))
  }

  test("long - collapse prohibited, fail") {
    fail[Opts]("--aopt=42 --bopt 7",LONG.withCollapsedValuesProhibited) match {
      case Seq(UnknownKey("--aopt=42"),MissingRequiredKey(spec)) =>
        spec.key should be ("aopt")
    }
  }

  test("long - collapse prohibited, pass") {
    parseOf[Opts]("--aopt 42 --bopt 7",LONG.withCollapsedValuesProhibited) should be ((Opts(42,7),Nil))
  }

  test("long - collapse required, fail") {
    fail[Opts]("--aopt=42 --bopt 7",LONG.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) => spec.key should be ("bopt")
    }
  }

  test("long - collapse required, fail with unknown key") {
    fail[Opts]("--aopt=42 --bopt=7 --blah=8",LONG.withCollapsedValuesRequired) should be (Seq(UnknownKey("--blah")))
  }

  test("long - collapse required, pass") {
    parseOf[Opts]("--aopt=42 --bopt=7",LONG.withCollapsedValuesRequired) should be ((Opts(42,7),Nil))
  }

  test("long - absent") {
    fail[Opts]("--aopt 42",LONG) match {
      case Seq(MissingRequiredKey(spec)) => spec.key should be ("bopt")
    }
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum --bopt=6",LONG) match {
      case Seq(InvalidValue(spec,"notanum",_)) =>
        spec.key should be ("aopt")
    }
  }

  test("long - require arg that is itself an option") {
    fail[Opts]("--aopt --bopt 7",LONG) match {
      case Seq(MissingValue(spec)) => spec.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
