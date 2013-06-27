package org.scalawag.druthers

import Parser.Flag

sealed trait UsageError

case class UnexpectedValue(flag:Flag,value:String) extends UsageError {
  override val toString = s"flag $flag doesn't take a value (got '$value')"
}

case class DuplicateValue(flag:Flag,existing:Any,value:Any) extends UsageError {
  override val toString = s"flag $flag specified twice (old:'$existing' new:'$value')"
}

case class InvalidValue(flag:Flag,value:String,reason:String) extends UsageError {
  override val toString = s"flag $flag has invalid value: $value ($reason)"
}

case class MissingValue(flag:Flag) extends UsageError {
  override val toString = s"flag $flag requires a value"
}

case class UnknownKey(key:String) extends UsageError {
  override val toString = s"unknown flag $key specified"
}

case class AmbiguousKey(key:String,possibilities:Seq[Flag]) extends UsageError {
  override val toString = s"ambiguous flag $key specified, could be any of ${possibilities.mkString(", ")}"
}

case class MissingRequiredKey(flag:Flag) extends UsageError {
  override val toString = s"flag $flag not specified"
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
