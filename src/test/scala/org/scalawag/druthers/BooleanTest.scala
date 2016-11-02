package org.scalawag.druthers

object BooleanTest {
  case class Opts(aopt:Boolean = false,
                  bopt:Boolean = false,
                  copt:Boolean = false)

  case class AmbiguousOpts(aa:Boolean,aaa:Boolean)

  case class AmbiguousNoOpts(a:Boolean,noA:Boolean)

  case class UnambiguousNoOpts(val aa:Option[Int],
                               val aaa:Boolean)
}

import BooleanTest._

class BooleanTest extends OptionsParserTest {

  test("short - present") {
    parseOf[Opts]("-a",SHORT) should be ((Opts(true),Nil))
  }

  test("short - present with trailing bare word") {
    parseOf[Opts]("-a bare",SHORT) should be ((Opts(true),List("bare")))
  }

  test("short - present with trailing option") {
    parseOf[Opts]("-a -b",SHORT) should be ((Opts(true,true),Nil))
  }

  test("short - absent") {
    parseOf[Opts]("",SHORT) should be ((Opts(),Nil))
  }

  test("short - cluster") {
    parseOf[Opts]("-ab",SHORT.withClustering) should be ((Opts(true,true),Nil))
  }

  test("short - clustering fails with clustering disabled") {
    fail[Opts]("-ab",SHORT) match {
      case Seq(UnexpectedValue(spec,"b")) => spec.key should be ("a")
    }
  }

  test("long - present") {
    parseOf[Opts]("--aopt",LONG) should be ((Opts(true),Nil))
  }

  test("long - absent") {
    parseOf[Opts]("",LONG) should be ((Opts(),Nil))
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=true",LONG) match {
      case Seq(UnexpectedValue(spec,"true")) => spec.key should be ("aopt")
    }
  }

  test("long - specify illegal value (empty)") {
    fail[Opts]("--aopt=",LONG) match {
      case Seq(UnexpectedValue(spec,"")) => spec.key should be ("aopt")
    }
  }

  test("long - no to negate") {
    parseOf[Opts]("--no-aopt --bopt --no-copt",LONG.withBooleansNegatedByNoPrefix) should be ((Opts(false,true,false),Nil))
  }

  test("long - unambiguous abbreviation") {
    parseOf[Opts]("--a",LONG.withAbbreviations) should be ((Opts(true),Nil))
  }

  test("long - unambiguous abbreviation, subset of another key") {
    parseOf[AmbiguousOpts]("--aa",LONG.withAbbreviations) should be ((AmbiguousOpts(true,false),Nil))
  }

  test("long - ambiguous abbreviation") {
    fail[AmbiguousOpts]("--a=",LONG.withAbbreviations) match {
      case Seq(AmbiguousKey("--a",Seq(spec1,spec2))) =>
        spec1.key should be ("aa")
        spec2.key should be ("aaa")
    }
  }

  test("long - negative unambiguous abbreviation") {
    parseOf[Opts]("--no-a --b",LONG.withAbbreviations.withBooleansNegatedByNoPrefix) should be ((Opts(false,true),Nil))
  }

  test("long - negative ambiguous abbreviation") {
    fail[AmbiguousOpts]("--no-a",LONG.withAbbreviations.withBooleansNegatedByNoPrefix) match {
      case Seq(AmbiguousKey("--no-a",Seq(spec1,spec2))) =>
        spec1.key should be ("aa")
        spec2.key should be ("aaa")
    }
  }

  test("long - positive keys take precedence") {
    parseOf[AmbiguousNoOpts]("--no-a",LONG.withBooleansNegatedByNoPrefix) should be ((AmbiguousNoOpts(false,true),Nil))
  }

  test("long - 'no' prefix doesn't conflict for non-boolean fields") {
    parseOf[UnambiguousNoOpts]("--no-a",LONG.withAbbreviations.withBooleansNegatedByNoPrefix) should be ((UnambiguousNoOpts(None,false),Nil))
  }
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
