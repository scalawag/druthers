import org.scalawag.timber.api.impl.EntryDispatcher
import org.scalawag.timber.api.impl.TimberConfiguration
import org.scalawag.timber.impl.dispatcher.SynchronousEntryDispatcher
import org.scalawag.timber.dsl._
import org.scalawag.timber.impl.formatter.DefaultEntryFormatter

object Timber extends TimberConfiguration {
  lazy val dispatcher = new SynchronousEntryDispatcher {
    configure { IN =>
      implicit val formatter = new DefaultEntryFormatter(headerOnEachLine = true)
      IN :: file("target/test.log")
    }
  }
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
