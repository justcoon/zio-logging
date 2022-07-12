package zio.logging

import zio.{ Cause, FiberId, FiberRefs, LogLevel, LogSpan, Trace, ZLogger }
import zio.logging.internal.{ LogAppender, LogMDCAppender }

trait LogMDC { self =>

  /**
   * A low-level interface which allows efficiently building a MDC with a
   * mutable builder.
   */
  private[logging] def unsafe(builder: LogMDCAppender): ZLogger[String, Unit]

  /**
   * Returns a new log MDC which concats both MDCs together
   */
  final def +(other: LogMDC): LogMDC =
    LogMDC.make { (builder, trace, fiberId, level, line, fiberRefs, spans, location, annotations) =>
      self.unsafe(builder)(trace, fiberId, level, line, fiberRefs, spans, location, annotations)
      other.unsafe(builder)(trace, fiberId, level, line, fiberRefs, spans, location, annotations)
    }

  /**
   * The alphanumeric version of the `+` operator.
   */
  final def concat(other: LogMDC): LogMDC =
    this + other

  final def toLogger: ZLogger[String, Map[String, String]] = (
    trace: Trace,
    fiberId: FiberId,
    logLevel: LogLevel,
    message: () => String,
    cause: Cause[Any],
    context: FiberRefs,
    spans: List[LogSpan],
    annotations: Map[String, String]
  ) => {
    val builder = collection.mutable.Map[String, String]()
    unsafe(LogMDCAppender.make(builder))(
      trace,
      fiberId,
      logLevel,
      message,
      cause,
      context,
      spans,
      annotations
    )
    builder.toMap
  }
}

object LogMDC {

  def make(
    append: (
      LogMDCAppender,
      Trace,
      FiberId,
      LogLevel,
      () => String,
      Cause[Any],
      FiberRefs,
      List[LogSpan],
      Map[String, String]
    ) => Any
  ): LogMDC = { (builder: LogMDCAppender) =>
    new ZLogger[String, Unit] {
      override def apply(
        trace: Trace,
        fiberId: FiberId,
        logLevel: LogLevel,
        message: () => String,
        cause: Cause[Any],
        context: FiberRefs,
        spans: List[LogSpan],
        annotations: Map[String, String]
      ): Unit = {
        append(builder, trace, fiberId, logLevel, message, cause, context, spans, annotations)
        ()
      }
    }
  }

  def logAnnotations: LogMDC =
    LogMDC.make { (builder, _, _, _, _, _, fiberRefs, _, _) =>
      fiberRefs
        .get(logContext)
        .foreach { context =>
          builder.append(context.asMap)
        }
      ()
    }

  def annotations: LogMDC =
    LogMDC.make { (builder, _, _, _, _, _, _, _, annotations) =>
      builder.append(annotations)
    }

  def allAnnotations: LogMDC = annotations + logAnnotations

  def label(label: => String, value: LogFormat): LogMDC =
    LogMDC.make { (builder, trace, fiberId, logLevel, message, context, spans, location, annotations) =>
      val valueBuilder = new StringBuilder()
      value.unsafeFormat(LogAppender.unstructured(valueBuilder.append(_)))(
        trace,
        fiberId,
        logLevel,
        message,
        context,
        spans,
        location,
        annotations
      )
      builder.append(label, valueBuilder.toString())
    }

  val thread = label("thread", LogFormat.fiberId)

  val level = label("level", LogFormat.level)

  val timestamp = label("timestamp", LogFormat.timestamp)

  val message = label("message", LogFormat.line)

  val error = label("error", LogFormat.cause)
}
