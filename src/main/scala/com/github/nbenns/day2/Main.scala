package com.github.nbenns.day2

import com.github.nbenns.shared.File.fromResource
import com.github.nbenns.shared.intcodecomputer.{Program, ProgramState}
import zio.*
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.core.file.Path

object Main extends App {
  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    fromResource("day2/input.txt")
      .map(file => Path(file))
      .flatMap(path =>
        AsynchronousFileChannel
          .open(path)
          .use(Program.loadMemory(2048))
      )
      .map(_.updated(1, 12L).updated(2, 2L))
      .map(ProgramState.deps)
      .flatMap(pstate => Program.runtimeEnv.provideCustom(pstate))
      .exitCode
}
