package zio.logging.example

import zio.logging.LogFormat
import zio.logging.backend.SLF4J
import zio.{Cause, EnvironmentTag, ExitCode, Runtime, Scope, URIO, ZIO, ZIOApp, ZIOAppArgs, ZLayer}

import java.util.concurrent.atomic.AtomicInteger

object Slf4jExampleApp2 extends ZIOApp {

  type Environment = PingService

  override val environmentTag: EnvironmentTag[Environment] = EnvironmentTag[Environment]

  override def bootstrap: ZLayer[ZIOAppArgs with Scope, Any, Environment] = logger >>> (LivePingService.layer ++ counter)

  val logger = Runtime.removeDefaultLoggers >>> SLF4J.slf4j(zio.LogLevel.Info, LogFormat.line |-| LogFormat.cause)

  private val counter = ZLayer.fromZIO { ZIO.logInfo("creating counter").as(new AtomicInteger()) }

  private def ping(address: String): URIO[PingService, Unit] =
    PingService
      .ping(address)
      .foldZIO(
        e => ZIO.logErrorCause(s"ping: $address - error", Cause.fail(e)),
        r => ZIO.logInfo(s"ping: $address - result: $r")
      )

  override def run: ZIO[Scope with PingService, Throwable, ExitCode] =
    for {
      _ <- ping("127.0.0.1")
      _ <- ping("x8.8.8.8")
    } yield ExitCode.success

}
