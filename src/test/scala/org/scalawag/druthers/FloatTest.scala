package org.scalawag.druthers

object FloatTest {
  case class Opts(val aopt:Float,
                  val bopt:Float)
}

import FloatTest._

class FloatTest extends OptionsParserTest {

  test("short - present") {
    parseOf[Opts]("-a 4.2 -b7.1",SHORT) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("short - collapse prohibited, fail") {
    fail[Opts]("-a 4.2 -b7.1",SHORT.withCollapsedValuesProhibited) match {
      case Seq(MissingValue(spec),UnknownKey("-7"),UnknownKey("-."),UnknownKey("-1")) =>
        spec.key should be ("b")
    }
  }

  test("short - collapse prohibited, pass") {
    parseOf[Opts]("-a 4.2 -b 7.1",SHORT.withCollapsedValuesProhibited) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("short - collapse required, fail") {
    fail[Opts]("-a 4.2 -b7.1",SHORT.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("a")
    }
  }

  test("short - collapse required, pass") {
    parseOf[Opts]("-a4.2 -b7.1",SHORT.withCollapsedValuesRequired) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("short - absent") {
    fail[Opts]("-a 4.2",SHORT) match {
      case Seq(MissingRequiredKey(spec)) => spec.key should be ("b")
    }
  }

  test("short - cluster, rest is arg (not other keys)") {
    parseOf[Opts]("-a 4.2 -b7.1",SHORT.withClustering) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("short - invalid delimited") {
    fail[Opts]("-a 4.2 -b x",SHORT) match {
      case Seq(InvalidValue(spec,"x",_)) => spec.key should be ("b")
    }
  }

  test("short - invalid non-delimited") {
    fail[Opts]("-a 4.2 -bx",SHORT) match {
      case Seq(InvalidValue(spec,"x",_)) => spec.key should be ("b")
    }
  }

  test("long - present") {
    parseOf[Opts]("--aopt 4.2 --bopt=7.1",LONG) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("long - collapse prohibited, fail") {
    fail[Opts]("--aopt=4.2 --bopt 7.1",LONG.withCollapsedValuesProhibited) match {
      case Seq(UnknownKey("--aopt=4.2"),MissingRequiredKey(spec)) =>
        spec.key should be ("aopt")
    }
  }

  test("long - collapse prohibited, pass") {
    parseOf[Opts]("--aopt 4.2 --bopt 7.1",LONG.withCollapsedValuesProhibited) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("long - collapse required, fail") {
    fail[Opts]("--aopt=4.2 --bopt 7.1",LONG.withCollapsedValuesRequired) match {
      case Seq(MissingValue(spec)) =>
        spec.key should be ("bopt")
    }
  }

  test("long - collapse required, pass") {
    parseOf[Opts]("--aopt=4.2 --bopt=7.1",LONG.withCollapsedValuesRequired) should be ((Opts(4.2f,7.1f),Nil))
  }

  test("long - absent") {
    fail[Opts]("--aopt 4.2",LONG) match {
      case Seq(MissingRequiredKey(spec)) => spec.key should be ("bopt")
    }
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=notanum --bopt 4",LONG) match {
      case Seq(InvalidValue(spec,"notanum",_)) =>
        spec.key should be ("aopt")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
