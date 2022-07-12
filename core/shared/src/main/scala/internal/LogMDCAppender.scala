package zio.logging.internal

private[logging] trait LogMDCAppender {

  def append(key: String, value: String): Unit

  def append(keyValues: Iterable[(String, String)]): Unit =
    keyValues.foreach {
      case (k, v) => append(k, v)
    }
}

private[logging] object LogMDCAppender {

  def make(appender: (String, String) => Unit): LogMDCAppender = new LogMDCAppender {
    override def append(key: String, value: String): Unit = appender(key, value)
  }

  def make(appender: collection.mutable.Map[String, String]): LogMDCAppender = new LogMDCAppender {
    override def append(key: String, value: String): Unit = appender.put(key, value)
  }
}
