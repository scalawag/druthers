import java.io.{FileWriter, File}
import org.scalawag.timber.api.impl.TimberConfiguration
import org.scalawag.timber.dsl._
import org.scalawag.timber.impl.formatter.DefaultEntryFormatter
import org.scalawag.timber.impl.receiver.{WriterReceiver, AutoFlush, FileAppender}

object Timber extends TimberConfiguration {
  override lazy val formatter = new DefaultEntryFormatter(headerOnEachLine = true)
  override lazy val receiver = new WriterReceiver(new FileWriter("target/test.log"),formatter) with AutoFlush
}

/* druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved */
