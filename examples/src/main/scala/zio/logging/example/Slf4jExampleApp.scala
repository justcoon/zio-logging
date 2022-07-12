package zio.logging.example

import zio.logging.{LogAnnotation, LogFormat}
import zio.logging.backend.SLF4J
import zio.{Cause, ExitCode, Runtime, Scope, Trace, URIO, ZIO, ZIOAppDefault, ZIOAspect}

object Slf4jExampleApp extends ZIOAppDefault {

  private val slf4jLogger = SLF4J.slf4j(
    zio.LogLevel.Debug,
    LogFormat.line |-| LogFormat.cause,
    _ match {
      case Trace(location, _, _) => location
      case _                 => "zio-slf4j-logger"
    }
  )

  private def ping(address: String): URIO[PingService, Unit] =
    PingService
      .ping(address)
      .foldZIO(
        e => ZIO.logErrorCause(s"ping: $address - error", Cause.fail(e)),
        r => ZIO.logInfo(s"ping: $address - result: $r") @@ ZIOAspect.annotated("username", "test_user") @@ LogAnnotation.UserId("1234")
      )

  override def run: ZIO[Scope, Any, ExitCode] = {

    val run: ZIO[PingService, Throwable, ExitCode] =
      for {
        _ <- ping("127.0.0.1")
        _ <- ping("x8.8.8.8")
      } yield ExitCode.success

    run.provide(
      LivePingService.layer ++ (Runtime.removeDefaultLoggers >>> slf4jLogger)
    )
  }

}
