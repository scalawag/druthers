package org.scalawag.druthers

import OptionsParser.OptionSpec
import CommandParser.Argument

sealed trait UsageError

case class UnexpectedValue(spec:OptionSpec,value:String) extends UsageError {
  override val toString = s"option ${spec.key} doesn't take a value (got '$value')"
}

case class DuplicateValue(spec:OptionSpec,existing:Any,value:Any) extends UsageError {
  override val toString = s"option ${spec.key} specified twice (old:'$existing' new:'$value')"
}

case class InvalidValue(spec:OptionSpec,value:String,reason:String) extends UsageError {
  override val toString = s"option ${spec.key} has invalid value: $value ($reason)"
}

case class MissingValue(spec:OptionSpec) extends UsageError {
  override val toString = s"option ${spec.key} requires a value"
}

case class UnknownKey(key:String) extends UsageError {
  override val toString = s"unknown option $key specified"
}

case class AmbiguousKey(key:String,possibilities:Seq[OptionSpec]) extends UsageError {
  override val toString = s"ambiguous option $key specified, could refer to any of ${possibilities.map(_.key).mkString(", ")}"
}

case class MissingRequiredKey(spec:OptionSpec) extends UsageError {
  override val toString = s"option ${spec.key} not specified"
}

case class MissingArgument(spec:Argument) extends UsageError {
  override val toString = s"argument '${spec.name}' not specified"
}

case class InvalidArgument(spec:Argument,value:String,reason:String) extends UsageError {
  override val toString = s"unable to find a suitable value for argument '${spec.name}': $value ($reason)"
}

case class ExtraneousValues(values:List[String]) extends UsageError {
  override val toString = s"extraneous arguments provided: ${values.mkString(" ")}"
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
