package com.github.nbenns.day5

import com.github.nbenns.shared.File.fromResource
import com.github.nbenns.shared.intcodecomputer.{Program, ProgramState}
import zio.*
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.core.file.Path

object Main extends App {
  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    fromResource("day5/input.txt")
      .map(file => Path(file))
      .flatMap( path =>
        AsynchronousFileChannel
          .open(path)
          .use(Program.loadMemory(2048))
      )
      .as(List(109L,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
      .map(ProgramState.deps)
      .flatMap(pstate => Program.runtimeEnv.provideCustom(pstate))
      .exitCode
}
