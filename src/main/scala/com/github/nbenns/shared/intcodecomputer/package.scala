package com.github.nbenns.shared

import cats.MonadError
import cats.implicits._
import com.github.nbenns.shared.Conversions.strToLong
import com.github.nbenns.shared.File.fileReadStreamCommaSep
import com.github.nbenns.shared.intcodecomputer.CPU.CPU
import com.github.nbenns.shared.intcodecomputer.Memory.Memory
import com.github.nbenns.shared.intcodecomputer.ProgramState._
import zio.ZIO
import zio.console.{Console, putStrLn}
import zio.interop.catz._
import zio.nio.channels.AsynchronousFileChannel
import zio.stream.Stream
import zio.stream.interop.catz._

package object intcodecomputer {
  type IProgram[+E, +A] = ZIO[Any, Option[E], A]
  type RProgram[-R, +E, +A] = ZIO[R, Option[E], A]
  type Program[+E, +A] = ZIO[Console with CPU with Memory, Option[E], A]

  object Program {
    def succeed[A](a: A): IProgram[Nothing, A] = ZIO.succeed(a)
    def fail[E](ex: E): IProgram[E, Nothing] = ZIO.fail(ex.some)
    def end: IProgram[Nothing, Nothing] = ZIO.fail(none)

    implicit def progErr[R, E]: MonadError[RProgram[R, E, *], E] = new MonadError[RProgram[R, E, *], E] {
      override def flatMap[A, B](fa: RProgram[R, E, A])(f: A => RProgram[R, E, B]): RProgram[R, E, B] =
        MonadError[ZIO[R, Option[E], *], Option[E]].flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => RProgram[R, E, Either[A, B]]): RProgram[R, E, B] =
        MonadError[ZIO[R, Option[E], *], Option[E]].tailRecM(a)(f)

      override def raiseError[A](e: E): RProgram[R, E, A] =
        MonadError[ZIO[R, Option[E], *], Option[E]].raiseError(e.some)

      override def handleErrorWith[A](fa: RProgram[R, E, A])(f: E => RProgram[R, E, A]): RProgram[R, E, A] =
        MonadError[ZIO[R, Option[E], *], Option[E]].handleErrorWith(fa){
          case Some(e) => f(e)
          case None    => Program.end
        }

      override def pure[A](x: A): RProgram[R, E, A] =
        MonadError[ZIO[R, Option[E], *], Option[E]].pure(x)
    }

    def loadMemory(chunkSize: Int)(fileChannel: AsynchronousFileChannel): ZIO[Console, Exception, List[Long]] =
      fileReadStreamCommaSep(chunkSize, fileChannel)
        .flatMap(strToLong[Stream])
        .runCollect
        .map(_.toList)
        .tap(_ => putStrLn("Loading Memory"))

    def runtimeEnv: ZIO[Console with CPU with Memory, Nothing, Unit] = {
      val result =
        putStrLn("Executing Program")
          .zipRight(CPU.run)
          .optional
          .refineToOrDie[Error]
          .catchAll(err => putStrLn(Error.asString(err)))

      val debug = ProgramState.debug.flatMap(putStrLn(_))

      result *> debug.ignore
    }
  }
}
