package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.Conversions.strToLong
import com.github.nbenns.shared.File.fileReadStreamCommaSep
import zio.*
import zio.Console.*
import zio.nio.channels.AsynchronousFileChannel

object Program {
  def succeed[A](a: A): ZIO[Any, Nothing, A] = ZIO.succeed(a)
  def fail[E](ex: E): ZIO[Any, Some[E], Nothing] = ZIO.fail(Some(ex))
  def end: ZIO[Any, None.type, Nothing] = ZIO.fail(None)
  def fromEither[E, A](either: Either[E, A]): ZIO[Any, Option[E], A] = either match {
    case Right(a) => Program.succeed(a)
    case Left(e)  => Program.fail(e)
  }

  def loadMemory(chunkSize: Int)(fileChannel: AsynchronousFileChannel): ZIO[Console, Exception, List[Long]] =
    fileReadStreamCommaSep(chunkSize, fileChannel)
      .mapZIO(a => ZIO.fromEither(strToLong(a)))
      .runCollect
      .map(_.toList)
      .zipLeft(printLine("Loading Memory"))

  def runtimeEnv: ZIO[Console & CPU & Memory, Nothing, Unit] = {
    val result =
      printLine("Executing Program")
        .asSomeError
        .zipRight(CPU.run)
        .unsome
        .refineToOrDie[Instruction.Error]
        .catchAll(err => printLine(err.asString))
        .ignore

    val debug = ProgramState.debug.flatMap(printLine(_))

    result *> debug.ignore
  }
}
