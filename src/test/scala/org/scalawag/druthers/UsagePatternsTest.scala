package org.scalawag.druthers

import scala.language.postfixOps

object UsagePatternsTest {

  // These classes illustrate how to use command-specific options classes that
  // subclass the global options.  The benefit here is that you don't have to
  // structure your command line so that it's parsing the correct options at
  // the correct time.  This allows you to mix bare words and options freely.
  // It prevents you from having something in a command-specific class collide
  // with something in the global options class.

  object Hierarchy {
    class GlobalOptions(val root:Option[String])
    class CreateOptions(override val root:Option[String],val name:String) extends GlobalOptions(root)
    class DeleteOptions(override val root:Option[String],val force:Boolean) extends GlobalOptions(root)

    case class CreateCommand(root:Option[String],name:String)
    case class DeleteCommand(root:Option[String],force:Boolean)
  }

  // These classes illustrate how to use command-specific options classes that
  // are independenty of the global options.  Note that CreateOptions.reverse
  // has the same key as GlobalOptions.root.  This is only possible because we'll
  // tell the parser's when to expect which options.  If you want to mix all the
  // options freely, use the Hierarchy style above.

  object Independent {
    class GlobalOptions(val root:Option[String])
    class CreateOptions(val name:String,val reverse:Boolean)
    class DeleteOptions(val force:Boolean)

    case class CreateCommand(root:Option[String],name:String,reverse:Boolean)
    case class DeleteCommand(root:Option[String],force:Boolean)
  }
}

import UsagePatternsTest._

class UsagePatternsTest extends OptionsParserTest {

  // This one is more efficient in that the global opts are only parsed once.
  // This comes at the expense of depth (nested cases) and arguably beauty (but
  // that's subjective).

  test("independent, cascading cases") {
    import Independent._
    new IndependentFixture {
      def parse(cmd:String) = split(cmd) match {
        case gparse(gopts,words) => words match {
          case "create" :: cparse(copts,Nil) =>
            CreateCommand(gopts.root,copts.name,copts.reverse)
          case "delete" :: dparse(dopts,Nil) =>
            DeleteCommand(gopts.root,dopts.force)
        }
      }
    } runTests
  }

  // This one will parse the global options for a "delete" command for both
  // cases (as opposed to the one above).  It's arguably easier to read, though,
  // and most programs will only process the command line options once (or at
  // least only as frequently as a human is typing them).

  test("independent, parallel cases") {
    import Independent._
    new IndependentFixture {
      def parse(cmd:String) = split(cmd) match {
        case gparse(gopts,"create" :: cparse(copts,Nil)) =>
          CreateCommand(gopts.root,copts.name,copts.reverse)
        case gparse(gopts,"delete" :: dparse(dopts,Nil)) =>
          DeleteCommand(gopts.root,dopts.force)
      }
    } runTests
  }

  // This is mostly ugly but if you name your parsers well, you may be able to
  // make it less so.  It's just here to demonstrate that you can use the parsers
  // as operators in the case statements if you choose.

  test("independent, operator-style") {
    import Independent._
    new IndependentFixture {
      def parse(cmd:String) = split(cmd) match {
        case gopts gparse "create" :: ( copts cparse Nil ) =>
          CreateCommand(gopts.root,copts.name,copts.reverse)
        case gopts gparse "delete" :: ( dopts dparse Nil ) =>
          DeleteCommand(gopts.root,dopts.force)
      }
    } runTests
  }

  trait IndependentFixture {
    import Independent._

    val config = SHORT.withStopAtFirstBareWord

    val gparse = new OptionsParser[GlobalOptions](config)
    val cparse = new OptionsParser[CreateOptions](config)
    val dparse = new OptionsParser[DeleteOptions](config)

    def parse(cmd:String):Any

    def runTests {
      intercept[MatchError](parse(""))
      intercept[UsageException](parse("-r home create"))
      parse("-r home create -n bob") should be (CreateCommand(Some("home"),"bob",false))
      parse("-r home create -r -n bob") should be (CreateCommand(Some("home"),"bob",true))
      parse("delete") should be (DeleteCommand(None,false))
    }
  }

  // This one illustrates that, using the hierarchical approach, one can make it
  // possible for the options to appear anywhere on the command line.  Note that
  // quiet mode is required on this one because, otherwise, the CreateOptions
  // parser will throw an exception when it sees a key (-f) that it does not grok.
  // It also has to parse out the options for each case in order to determine
  // what the first bare word is.

  test("hierarchical, flexible positioning") {
    import Hierarchy._

    val config = SHORT.withQuietMode

    val cparse = new OptionsParser[CreateOptions](config)
    val dparse = new OptionsParser[DeleteOptions](config)

    def parse(cmd:String) = split(cmd) match {
      case cparse(copts,"create" :: Nil) =>
        CreateCommand(copts.root,copts.name)
      case dparse(dopts,"delete" :: Nil) =>
        DeleteCommand(dopts.root,dopts.force)
    }

    parse("-r home create -n bob") should be (CreateCommand(Some("home"),"bob"))
    parse("create -n bob -r home") should be (CreateCommand(Some("home"),"bob"))
    parse("-f delete") should be (DeleteCommand(None,true))
    parse("delete -f") should be (DeleteCommand(None,true))
  }

  // Finally, you can use the OptionsParser.parse() method directly if that fits your
  // use case better.

  test("direct") {
    import Hierarchy._

    val parser = new OptionsParser[CreateOptions]

    val args = split("-r home create -n name another")

    val (opts,remains) = parser.parse(args)

    opts.root should be (Some("home"))
    opts.name should be ("name")
    remains should be (List("create","another"))
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
