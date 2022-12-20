package zio

import zio.*
import zio.stream.*

import scala.io.Source

trait AdventOfCodeApp extends ZIOAppDefault {

  protected val puzzleData = ZStream
    .acquireReleaseWith(ZIO.attempt(Source.fromURL(getClass.getResource("puzzle-input.txt"))))(source => ZIO.succeed(source.close()))
    .flatMap(source => ZStream.fromIterator(source.getLines()))
    .orElseFail(new Exception(getClass.getPackageName.replaceAll("\\.", "/") + "/puzzle-input.txt resource not found"))

  protected def aocLogging[T](logLine:String, appendOutput: Boolean=false)(tuple: (Duration, T)):ZIO[Any, Throwable, T] = tuple match
    case (duration, output) => Console.printLine(logLine + (if appendOutput then output.toString else "") + s" [${duration.toMillis}ms]").as(output)

  def program: ZIO[Any, Throwable, Unit]

  override def run: ZIO[Any, Any, Any] = program.timed.flatMap(aocLogging("Completed"))


}
