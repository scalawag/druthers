import org.scalawag.timber.api.impl.TimberConfiguration
import org.scalawag.timber.dsl._
import org.scalawag.timber.impl.formatter.DefaultEntryFormatter

object Timber extends TimberConfiguration {
  override val formatter = new DefaultEntryFormatter(headerOnEachLine = true)
  override val receiver = file("target/test.log")(formatter)
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
