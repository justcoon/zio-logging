package zio.logging.example

import zhttp.http.{Http, Request}
import zio._
import zhttp.http._
import zhttp.http.middleware.HttpMiddleware
import zhttp.service.Server
import zio.{LogLevel, Runtime, Trace}
import zio.logging.LogFormat._
import zio.logging.backend.SLF4J

object ZHttpApp extends ZIOAppDefault {

  object Logger {
    private val logFormat = line |-| cause

    val layerThatWorks = Runtime.removeDefaultLoggers >>> SLF4J.slf4j(LogLevel.All, logFormat, (trace: Trace) => s"$trace")

    val layerThatDoesntWork = Runtime.removeDefaultLoggers >>> SLF4J.slf4j(LogLevel.All, logFormat, (trace: Trace) => trace.toString) //gets error. Doesn't get error if I do s"trace", but then it just says null.

  }

  object CacheControlHeader {

    val middleware: HttpMiddleware[Any, Throwable] =
      Middleware.whenHeader(
        headers => headers.cacheControl.isEmpty,
        Middleware.updateResponse { response =>
          response.addHeader(HeaderNames.cacheControl, "no-cache, private, no-store, post-check=0, pre-check=0, must-revalidate")
        }
      )

  }

  def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val chosenLayer = Logger.layerThatWorks

    val httpApp = Http.collectHttp[Request] {
      case _ -> "" /: "_status" /: path =>
        Http.collect[Request] {
          case Method.GET -> ~~ / "health" => Response.ok
        }.setPath(path)

    } @@ CacheControlHeader.middleware @@ Middleware.timeout(15.seconds) //Both of these are required, seems like the issue comes when there is an interruption. You can swap layers to see the actual logstatement.
    val appLogic = for {
      _ <- ZIO.logInfo("Booting reproducer")
      _ <- Server(httpApp.provideSomeLayer[Any, Any, Throwable](chosenLayer)).withBinding("0.0.0.0", 1337).startDefault[Any]
    } yield ()

    appLogic.provide(chosenLayer)

  }
}