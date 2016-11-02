package org.scalawag.druthers

import scala.reflect.runtime.universe.TypeTag

abstract class OptionsParserTest extends ParserTest {

  protected[this] def parseOf[T:TypeTag](args:String,config:ParserConfiguration = null) = {
    val parser = new OptionsParser[T](config)
    parser.parse(split(args))
  }

  protected[this] def fail[T:TypeTag](args:String,config:ParserConfiguration = null) = {
    intercept[UsageException]((new OptionsParser[T](config)).parse(split(args))).errors
  }

}

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
