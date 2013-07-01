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
    succeed[Opts]("-a",SHORT) { case(opts,remains) =>
      opts should be (Opts(true))
      remains should be (Nil)
    }
  }

  test("short - present with trailing bare word") {
    succeed[Opts]("-a bare",Opts(true),"bare",SHORT)
  }

  test("short - present with trailing option") {
    succeed[Opts]("-a -b",Opts(true,true),"",SHORT)
  }

  test("short - absent") {
    succeed[Opts]("",Opts(),"",SHORT)
  }

  test("short - cluster") {
    succeed[Opts]("-ab",Opts(true,true),"",SHORT.withClustering)
  }

  test("short - clustering fails with clustering disabled") {
    fail[Opts]("-ab",SHORT) {
      case Seq(UnexpectedValue(spec,"b")) =>
        spec.key should be ("a")
    }
  }

  test("long - present") {
    succeed[Opts]("--aopt",Opts(true),"",LONG)
  }

  test("long - absent") {
    succeed[Opts]("",Opts(),"",LONG)
  }

  test("long - specify illegal value") {
    fail[Opts]("--aopt=true",LONG) {
      case Seq(UnexpectedValue(spec,"true")) =>
        spec.key should be ("aopt")
    }
  }

  test("long - specify illegal value (empty)") {
    fail[Opts]("--aopt=",LONG) {
      case Seq(UnexpectedValue(spec,"")) =>
        spec.key should be ("aopt")
    }
  }

  test("long - no to negate") {
    succeed[Opts]("--no-aopt --bopt --no-copt",Opts(false,true,false),"",LONG.withBooleansNegatedByNoPrefix)
  }

  test("long - unambiguous abbreviation") {
    succeed[Opts]("--a",Opts(true),"",LONG.withAbbreviations)
  }

  test("long - unambiguous abbreviation, subset of another key") {
    succeed[AmbiguousOpts]("--aa",AmbiguousOpts(true,false),"",LONG.withAbbreviations)
  }

  test("long - ambiguous abbreviation") {
    fail[AmbiguousOpts]("--a=",LONG.withAbbreviations) {
      case Seq(AmbiguousKey("--a",Seq(spec1,spec2))) =>
        spec1.key should be ("aa")
        spec2.key should be ("aaa")
    }
  }

  test("long - negative unambiguous abbreviation") {
    succeed[Opts]("--no-a --b",Opts(false,true),"",LONG.withAbbreviations.withBooleansNegatedByNoPrefix)
  }

  test("long - negative ambiguous abbreviation") {
    fail[AmbiguousOpts]("--no-a",LONG.withAbbreviations.withBooleansNegatedByNoPrefix) {
      case Seq(AmbiguousKey("--no-a",Seq(spec1,spec2))) =>
        spec1.key should be ("aa")
        spec2.key should be ("aaa")
    }
  }

  ignore("long - positive keys take precedence") {
    succeed[AmbiguousNoOpts]("--no-a",AmbiguousNoOpts(false,true),"",LONG.withBooleansNegatedByNoPrefix)
  }

  test("long - 'no' prefix doesn't conflict for non-boolean fields") {
    succeed[UnambiguousNoOpts]("--no-a",UnambiguousNoOpts(None,false),"",LONG.withAbbreviations.withBooleansNegatedByNoPrefix)
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
