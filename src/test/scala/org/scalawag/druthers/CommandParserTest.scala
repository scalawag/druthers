package org.scalawag.druthers

import scala.reflect.runtime.universe.TypeTag

object CommandParserTest {
  case class ABC(a:Int,b:String,c:Option[String],d:Seq[String])

  case class GreedyOption(a:Option[String],b:Seq[String])
  case class GreedySeq(a:Seq[String],b:Option[String])

  case class GreedySeqWithRequired(a:Seq[String],b:String)
  case class GreedyOptionWithRequired(a:Option[String],b:String)

  case class GreedAbatement(ints:Seq[Int],floats:Seq[Float],strings:Seq[String])

  case class GlobalOptions(root:Option[String])
  case class CommandOptions(force:Boolean,reverse:Boolean)

  case class FancyCommand(gopts:GlobalOptions,cmd:String,copts:CommandOptions,args:Seq[String])

  case class DefaultParameters(a:String = "A",b:Int = 2,c:Float = 3f,d:Seq[String] = Seq("x","y","z"),e:Option[Int] = Some(42))
  case class DefaultParametersWithRequired(a:String = "A",b:Int)
}

import CommandParserTest._

class CommandParserTest extends ParserTest {

  test("keywords only - too few arguments") {
    fail[ABC]("8") {
      case Seq(MissingArgument(arg)) =>
        arg.name should be ("b")
    }
  }

  test("keywords only - way too few arguments") {
    fail[ABC]("") {
      case Seq(MissingArgument(arg)) =>
        arg.name should be ("a")
        // TODO: should this report both missing arguments?
    }
  }

  test("keywords only - invalid value") {
    fail[ABC]("a") {
      case Seq(InvalidArgument(arg,"a",_)) =>
        arg.name should be ("a")
    }
  }

  test("Options are greedy") {
    parseOf[GreedyOption]("a b") should be {
      GreedyOption(Some("a"),Seq("b"))
    }
  }

  test("Seqs are greedy") {
    parseOf[GreedySeq]("a b") should be (GreedySeq(Seq("a","b"),None))
  }

  test("greedy Option saves one at the end for required argument") {
    parseOf[GreedyOptionWithRequired]("b") should be {
      GreedyOptionWithRequired(None,"b")
    }
  }

  test("greedy Seq saves one at the end for required argument") {
    parseOf[GreedySeqWithRequired]("a b") should be (GreedySeqWithRequired(Seq("a"),"b"))
  }

  test("greed abates when invalid arguments appear") {
    parseOf[GreedAbatement]("8 7 6 5 4.0 3.0 2.0 one zero") should be {
      GreedAbatement(Seq(8,7,6,5),Seq(4f,3f,2f),Seq("one","zero"))
    }
  }

  test("greed permits lowered standards to make the whole command work (float eats int)") {
    parseOf[GreedAbatement]("8.0 7 6 5 4 3 2.0 one zero") should be {
      GreedAbatement(Seq(),Seq[Float](8,7,6,5,4,3,2),Seq("one","zero"))
    }
  }

  test("fancy (two sets of Options) command") {
    val parser = new CommandParser2[FancyCommand,GlobalOptions,CommandOptions]

    parser.parse(split("-r home create -f a1 a2 -r a3")) should be {
      FancyCommand(GlobalOptions(Some("home")),"create",CommandOptions(true,true),Seq("a1","a2","a3"))
    }
  }

  test("fancy (two sets of Options) command, stopping at first bare word") {
    val parser = new CommandParser2[FancyCommand,GlobalOptions,CommandOptions](SHORT.withStopAtFirstBareWord)

    parser.parse(split("-r home create -f a1 a2 -r a3")) should be {
      FancyCommand(GlobalOptions(Some("home")),"create",CommandOptions(true,false),Seq("a1","a2","-r","a3"))
    }
  }

  test("default parameters") {
    val parser = new CommandParser[DefaultParameters]

    parser.parse(Nil) should be {
      DefaultParameters()
    }
  }

  test("default parameters are greedy") {
    val parser = new CommandParser[DefaultParameters]

    parser.parse(split("8")) should be {
      DefaultParameters("8")
    }
  }

  test("default parameters are greedy but will give up parameters to meet requirements") {
    val parser = new CommandParser[DefaultParametersWithRequired]

    parser.parse(split("8")) should be {
      DefaultParametersWithRequired(b = 8)
    }
  }

  private[this] def parseOf[T:TypeTag](args:String,config:ParserConfiguration = null) = {
    val parser = new CommandParser[T](config)
    val cmd = parser.parse(split(args))
    cmd
  }

  private[this] def fail[T:TypeTag](args:String,config:ParserConfiguration = null)(fn:PartialFunction[Seq[UsageError],Unit]) {
    fn(intercept[UsageException]((new CommandParser[T](config)).parse(split(args))).errors)
  }

}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
