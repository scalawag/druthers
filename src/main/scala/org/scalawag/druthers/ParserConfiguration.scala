package org.scalawag.druthers

object ParserConfiguration

abstract class ParserConfiguration(val useLongKeys:Boolean,
                                   val optionPrefix:String,
                                   val stopAtFirstBareWord:Boolean = false,
                                   val throwUsageErrors:Boolean = true,
                                   val multipleValueDelimiter:Option[String] = None,
                                   // LongOpts-specific
                                   val equalsDelimitsValue:Option[Boolean] = None,
                                   val booleanNegatedByNo:Boolean = false,
                                   val abbreviation:Boolean = false,
                                   // ShortOpts-specific
                                   val clustering:Boolean = false,
                                   val spaceDelimitsValue:Option[Boolean] = None) {
  val spaceMustDelimitValue = spaceDelimitsValue.getOrElse(false)
  val spaceMayDelimitValue = spaceDelimitsValue.getOrElse(true)
}

case class LongOptions(override val stopAtFirstBareWord:Boolean = false,
                       override val throwUsageErrors:Boolean = true,
                       override val multipleValueDelimiter:Option[String] = None,
                       override val equalsDelimitsValue:Option[Boolean] = None,
                       override val booleanNegatedByNo:Boolean = false,
                       override val abbreviation:Boolean = false)
  extends ParserConfiguration(true,
                              "--",
                              stopAtFirstBareWord,
                              throwUsageErrors,
                              multipleValueDelimiter,
                              equalsDelimitsValue,
                              booleanNegatedByNo,
                              abbreviation,
                              false, // ignored for LongOpts
                              None) // ignored for LongOpts
{
  def withBooleanNegatedByNo(value:Boolean = true) =
    this.copy(booleanNegatedByNo = value)
  def withAbbreviation(value:Boolean = true) =
    this.copy(abbreviation = value)
  def withEqualDelimitsValue(value:Option[Boolean] = Some(true)) =
    this.copy(equalsDelimitsValue = value)

  def withStopAtFirstBareWord:LongOptions =
    withStopAtFirstBareWord(true)
  def withStopAtFirstBareWord(value:Boolean = true):LongOptions =
    this.copy(stopAtFirstBareWord = value)
  def withThrowUsageErrors(value:Boolean = true) =
    this.copy(throwUsageErrors = value)
  def withMultipleValueDelimiter(delimiter:String) =
    this.copy(multipleValueDelimiter = Some(delimiter))
}

case class ShortOptions(override val stopAtFirstBareWord:Boolean = false,
                        override val throwUsageErrors:Boolean = true,
                        override val multipleValueDelimiter:Option[String] = None,
                        override val clustering:Boolean = false,
                        override val spaceDelimitsValue:Option[Boolean] = None)
  extends ParserConfiguration(false,
                              "-",
                              stopAtFirstBareWord,
                              throwUsageErrors,
                              multipleValueDelimiter,
                              None, // ignored for ShortOpts
                              false, // ignored for ShortOpts
                              false, // ignored for ShortOpts
                              clustering,
                              spaceDelimitsValue)
{
  def withClustering(value:Boolean = true) =
    this.copy(clustering = value)
  // TODO: make this default more predictable/explicable (maybe add enum?)
  def withSpaceDelimitsValue(value:Option[Boolean] = Some(true)) =
    this.copy(spaceDelimitsValue = value)

  def withStopAtFirstBareWord(value:Boolean = true) =
    this.copy(stopAtFirstBareWord = value)
  def withThrowUsageErrors(value:Boolean = true) =
    this.copy(throwUsageErrors = value)
  def withMultipleValueDelimiter(delimiter:String) =
    this.copy(multipleValueDelimiter = Some(delimiter))
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
