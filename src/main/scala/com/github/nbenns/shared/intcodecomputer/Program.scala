package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.Conversions.strToLong
import com.github.nbenns.shared.File.fileReadStreamCommaSep
import zio.{Console, Has, ZIO}
import Console.*
import zio.nio.channels.AsynchronousFileChannel

type IProgram[+E, +A] = ZIO[Any, Option[E], A]
type RProgram[-R, +E, +A] = ZIO[R, Option[E], A]
type Program[+E, +A] = ZIO[Has[Console] & Has[CPU] & Has[Memory], Option[E], A]

object Program {
  def succeed[A](a: A): IProgram[Nothing, A] = ZIO.succeed(a)
  def fail[E](ex: E): IProgram[E, Nothing] = ZIO.fail(Some(ex))
  def end: IProgram[Nothing, Nothing] = ZIO.fail(None)
  def fromEither[E, A](either: Either[E, A]): IProgram[E, A] = either match {
    case Right(a) => Program.succeed(a)
    case Left(e)  => Program.fail(e)
  }

  def loadMemory(chunkSize: Int)(fileChannel: AsynchronousFileChannel): ZIO[Has[Console], Exception, List[Long]] =
    fileReadStreamCommaSep(chunkSize, fileChannel)
      .mapZIO(a => ZIO.fromEither(strToLong(a)))
      .runCollect
      .map(_.toList)
      .zipLeft(printLine("Loading Memory"))

  def runtimeEnv: ZIO[Has[Console] & Has[CPU] & Has[Memory], Nothing, Unit] = {
    val result =
      printLine("Executing Program")
        .asSomeError
        .zipRight(CPU.run)
        .unoption
        .refineToOrDie[Instruction.Error]
        .catchAll(err => printLine(err.asString))
        .ignore

    val debug = ProgramState.debug.flatMap(printLine(_))

    result *> debug.ignore
  }
}
