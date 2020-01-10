package com.github.nbenns.day2

import com.github.nbenns.shared.File.fromResource
import com.github.nbenns.shared.intcodecomputer.Program
import zio._
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.file.Path

object Main extends App {
  val file: String = fromResource("day2/input.txt")

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    AsynchronousFileChannel
      .open(Path(file))
      .use(Program.loadMemory(2048))
      .map(_.updated(1, 12L).updated(2, 2L))
      .flatMap(Dependencies.apply)
      .flatMap(Program.runtimeEnv.provide)
      .orDie
      .as(0)
}
