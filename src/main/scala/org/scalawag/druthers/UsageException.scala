package org.scalawag.druthers

case class UsageException(errors:Seq[UsageError]) extends Exception(errors.mkString("\n"))

/* druthers -- Copyright 2013-2016 Justin Patterson -- All Rights Reserved */
