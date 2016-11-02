package org.scalawag.druthers

object ParserConfiguration {
  val withLongKeys = LongOptions()
  val withShortKeys = ShortOptions()
}

/**
 *  @param useLongKeys use long keys (field name, decamelized)
 *                     instead of short keys (first character of field name)
 *  @param optionPrefix the prefix that indicates an option key is coming
 *  @param stopAtFirstBareWord stop parsing options when the first bare word is encountered,
 *                             otherwise, continue looking for options after the bare word
 *  @param quietMode suppress UsageExceptions (can be useful during pattern matching)
 *  @param valueDelimiter regexp to split multiple values in a single argument
 *  @param collapsedValues Some(true) means that all values must be collapsed with their key
 *                         in a single argument (with '=' delimiting for long key mode and
 *                         nothing delimiting in short key mode).  Some(false) means that
 *                         collapsing is prohibited.  None means either style is allowed.
 * @param booleansNegatedByNoPrefix whether prefixing a boolean key with "no-" will cause that
 *                                  option to be set to false (long key mode only)
 * @param abbreviations whether a long key can be specified with an unambiguous substring
 * @param clustering whether multiple short keys can be clustered into a single argument
 */

sealed abstract class ParserConfiguration protected (val useLongKeys:Boolean,
                                                     val optionPrefix:String,
                                                     val stopAtFirstBareWord:Boolean = false,
                                                     val quietMode:Boolean = false,
                                                     val valueDelimiter:Option[String] = None,
                                                     val collapsedValues:Option[Boolean] = None,
                                                     // LongOpts-specific
                                                     val booleansNegatedByNoPrefix:Boolean = false,
                                                     val abbreviations:Boolean = false,
                                                     // ShortOpts-specific
                                                     val clustering:Boolean = false) {
  require(!clustering || !useLongKeys,"can't use clustering with long keys")
  require(!booleansNegatedByNoPrefix || useLongKeys,"can't use no- prefixes with short keys")
  require(!abbreviations || useLongKeys,"can't use abbreviations with short keys")

  val mayCollapseValues = collapsedValues.getOrElse(true)
  val mustCollapseValues = collapsedValues.getOrElse(false)
  val mayNotCollapseValues = !mustCollapseValues
  val mustNotCollapseValues = !mayCollapseValues
}

case class LongOptions(override val stopAtFirstBareWord:Boolean = false,
                       override val quietMode:Boolean = false,
                       override val valueDelimiter:Option[String] = None,
                       override val collapsedValues:Option[Boolean] = None,
                       override val booleansNegatedByNoPrefix:Boolean = false,
                       override val abbreviations:Boolean = false)
  extends ParserConfiguration(true,
                              "--",
                              stopAtFirstBareWord,
                              quietMode,
                              valueDelimiter,
                              collapsedValues,
                              booleansNegatedByNoPrefix,
                              abbreviations,
                              false) // ignored for LongOpts
{
  def withBooleansNegatedByNoPrefix = this.copy(booleansNegatedByNoPrefix = true)
  def withAbbreviations = this.copy(abbreviations = true)

  def withCollapsedValuesRequired = this.copy(collapsedValues = Some(true))
  def withCollapsedValuesProhibited = this.copy(collapsedValues = Some(false))
  def withStopAtFirstBareWord = this.copy(stopAtFirstBareWord = true)
  def withQuietMode = this.copy(quietMode = true)
  def withValueDelimiter(delimiter:String) = this.copy(valueDelimiter = Some(delimiter))
}

case class ShortOptions(override val stopAtFirstBareWord:Boolean = false,
                        override val quietMode:Boolean = false,
                        override val valueDelimiter:Option[String] = None,
                        override val collapsedValues:Option[Boolean] = None,
                        override val clustering:Boolean = false)
  extends ParserConfiguration(false,
                              "-",
                              stopAtFirstBareWord,
                              quietMode,
                              valueDelimiter,
                              collapsedValues,
                              false, // ignored for ShortOpts
                              false, // ignored for ShortOpts
                              clustering)
{
  def withClustering = this.copy(clustering = true)

  def withCollapsedValuesRequired = this.copy(collapsedValues = Some(true))
  def withCollapsedValuesProhibited = this.copy(collapsedValues = Some(false))
  def withStopAtFirstBareWord = this.copy(stopAtFirstBareWord = true)
  def withQuietMode = this.copy(quietMode = true)
  def withValueDelimiter(delimiter:String) = this.copy(valueDelimiter = Some(delimiter))
}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
