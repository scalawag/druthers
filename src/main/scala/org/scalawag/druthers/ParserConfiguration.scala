package org.scalawag.druthers

object ParserConfiguration

abstract class ParserConfiguration(val useLongKeys:Boolean,
                                   val optionPrefix:String,
                                   val stopAtFirstBareWord:Boolean = false,
                                   val throwUsageErrors:Boolean = true,
                                   val multipleValueDelimiter:Option[String] = None,
                                   val collapseValues:Option[Boolean] = None,
                                   // LongOpts-specific
                                   val booleanNegatedByNo:Boolean = false,
                                   val abbreviation:Boolean = false,
                                   // ShortOpts-specific
                                   val clustering:Boolean = false) {
  val mayCollapseValues = collapseValues.getOrElse(true)
  val mustCollapseValues = collapseValues.getOrElse(false)
  val mayNotCollapseValues = !mustCollapseValues
  val mustNotCollapseValues = !mayCollapseValues
}

case class LongOptions(override val stopAtFirstBareWord:Boolean = false,
                       override val throwUsageErrors:Boolean = true,
                       override val multipleValueDelimiter:Option[String] = None,
                       override val collapseValues:Option[Boolean] = None,
                       override val booleanNegatedByNo:Boolean = false,
                       override val abbreviation:Boolean = false)
  extends ParserConfiguration(true,
                              "--",
                              stopAtFirstBareWord,
                              throwUsageErrors,
                              multipleValueDelimiter,
                              collapseValues,
                              booleanNegatedByNo,
                              abbreviation,
                              false) // ignored for LongOpts
{
  def withBooleanNegatedByNo(value:Boolean = true) =
    this.copy(booleanNegatedByNo = value)
  def withAbbreviation(value:Boolean = true) =
    this.copy(abbreviation = value)
  def withCollapseValues(value:Option[Boolean] = Some(true)) =
    this.copy(collapseValues = value)

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
                        override val collapseValues:Option[Boolean] = None)
  extends ParserConfiguration(false,
                              "-",
                              stopAtFirstBareWord,
                              throwUsageErrors,
                              multipleValueDelimiter,
                              collapseValues,
                              false, // ignored for ShortOpts
                              false, // ignored for ShortOpts
                              clustering)
{
  def withClustering(value:Boolean = true) =
    this.copy(clustering = value)
  // TODO: make this default more predictable/explicable (maybe add enum?)
  def withCollapseValues(value:Option[Boolean] = Some(true)) =
    this.copy(collapseValues = value)

  def withStopAtFirstBareWord(value:Boolean = true) =
    this.copy(stopAtFirstBareWord = value)
  def withThrowUsageErrors(value:Boolean = true) =
    this.copy(throwUsageErrors = value)
  def withMultipleValueDelimiter(delimiter:String) =
    this.copy(multipleValueDelimiter = Some(delimiter))
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
