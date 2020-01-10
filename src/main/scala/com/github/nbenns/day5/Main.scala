package com.github.nbenns.day5

import com.github.nbenns.day2.Dependencies
import com.github.nbenns.shared.File.fromResource
import com.github.nbenns.shared.intcodecomputer.Program
import zio._
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.file.Path

object Main extends App {
  val file: String = fromResource("day5/input.txt")

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    AsynchronousFileChannel
      .open(Path(file))
      .use(Program.loadMemory(2048))
      .as(List(109L,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
      .flatMap(Dependencies.apply)
      .flatMap(Program.runtimeEnv.provide)
      .orDie
      .as(0)
}
