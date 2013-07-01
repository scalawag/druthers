package org.scalawag.druthers

import OptionsParser.Flag
import CommandParser.Argument

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

case class MissingArgument(argument:Argument) extends UsageError {
  override val toString = s"argument $argument not specified"
}

case class InvalidArgument(argument:Argument,value:String,reason:String) extends UsageError {
  override val toString = s"unable to find a suitable value for argument $argument: $value ($reason)"
}

case class ExtraneousValues(values:List[String]) extends UsageError {
  override val toString = s"extraneous values provided: ${values.mkString(" ")}"
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
