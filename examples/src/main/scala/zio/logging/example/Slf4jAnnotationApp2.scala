package zio.logging.example

import zio.logging.backend.SLF4J
import zio.logging.LogFormat
import zio.{ExitCode, LogLevel, Runtime, Scope, ZIO, ZIOAppDefault, _}

import java.util.UUID


trait Tracing {
  def getCurrentSpan(): UIO[String]
}

final class LiveTracing extends Tracing {
  override def getCurrentSpan(): UIO[String] = ZIO.succeed(UUID.randomUUID().toString)
}

object LiveTracing {
  val layer: ULayer[Tracing] = ZLayer.succeed(new LiveTracing)
}

object Slf4jAnnotationApp2 extends ZIOAppDefault {

  def currentSpanAspect(key: String): ZIOAspect[Nothing, Tracing, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Tracing, Nothing, Any, Nothing, Any] {
      def apply[R <: Tracing, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = {
        ZIO.serviceWithZIO[Tracing] { tracing =>
          tracing.getCurrentSpan().flatMap {
            span => ZIO.logAnnotate(key, span)(zio)
          }
        }
      }
    }

  private val logger =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j(
      LogLevel.Info,
      LogFormat.annotation("trace_id") |-| LogFormat.annotation(
        "user"
      ) |-| LogFormat.line |-| LogFormat.cause
    )

  private val users = List.fill(2)(UUID.randomUUID())

  override def run: ZIO[Scope, Any, ExitCode] = {
    val r: ZIO[Tracing, Any, ExitCode] = for {
      _ <- ZIO.foreachPar(users) { uId => {
        ZIO.logInfo("Starting operation") *>
          ZIO.sleep(500.millis) *>
          ZIO.logInfo("Stopping operation")
      } @@ ZIOAspect.annotated("user", uId.toString)
      } @@ currentSpanAspect("trace_id")
      _ <- ZIO.logInfo("Done")
    } yield ExitCode.success
    r.provide(logger ++ LiveTracing.layer)
  }

}
