package com.github.nbenns.day1

import com.github.nbenns.shared.Conversions.strToLong
import com.github.nbenns.shared.File.{fileReadStreamByLine, fromResource}
import zio._
import zio.console._
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.core.file.Path
import zio.stream.Stream
import zio.stream.interop.catz._

object Main extends App {
  def calculateFuel(method: Long => Long, chunkSize: Int)(fileChannel: AsynchronousFileChannel): IO[Exception, Long] =
    fileReadStreamByLine(chunkSize, fileChannel)
      .flatMap(strToLong[Stream])
      .map(method)
      .runSum

  val file: String = fromResource("day1/input.txt")

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    AsynchronousFileChannel
      .open(Path(file))
      .use(calculateFuel(Part2, 20))
      .map(_.toString)
      .flatMap(s => putStrLn(s))
      .exitCode
}
