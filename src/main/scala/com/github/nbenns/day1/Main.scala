package com.github.nbenns.day1

import com.github.nbenns.shared.Conversions.strToLong
import com.github.nbenns.shared.File.{fileReadStreamByLine, fromResource}
import zio.*
import zio.Console.*
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.core.file.Path

object Main extends App {
  def calculateFuel(method: Long => Long, chunkSize: Int)(fileChannel: AsynchronousFileChannel): IO[Exception, Long] =
    fileReadStreamByLine(chunkSize, fileChannel)
      .mapZIO(str => ZIO.fromEither(strToLong(str)))
      .map(method)
      .runSum

  val file: Task[String] = fromResource("day1/input.txt")

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    fromResource("day1/input.txt")
      .map(file => Path(file))
      .flatMap(path =>
        AsynchronousFileChannel
          .open(path)
          .use(calculateFuel(Part2, 20))
      )
      .map(_.toString)
      .flatMap(s => printLine(s))
      .exitCode
}
