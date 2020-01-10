package com.github.nbenns.shared

import cats.MonadError
import cats.implicits._
import com.github.nbenns.shared.Conversions.strToLong
import com.github.nbenns.shared.File.fileReadStreamCommaSep
import com.github.nbenns.shared.intcodecomputer.ProgramState.PrgState
import zio.ZIO
import zio.console.{Console, putStrLn}
import zio.interop.catz._
import zio.nio.channels.AsynchronousFileChannel
import zio.stream.Stream
import zio.stream.interop.catz._

package object intcodecomputer {
  type Program[+E, +A] = ZIO[Console with ProgramState, Option[E], A]

  object Program {
    def succeed[A](a: A): Program[Nothing, A] = ZIO.succeed(a)
    def fail[E](ex: E): Program[E, Nothing] = ZIO.fail(ex.some)
    def end: Program[Nothing, Nothing] = ZIO.fail(none)

    def state: Program[Nothing, PrgState] = ZIO.access[ProgramState](_.programState)

    implicit def progErr[E]: MonadError[Program[E, *], E] = new MonadError[Program[E, *], E] {
      override def flatMap[A, B](fa: Program[E, A])(f: A => Program[E, B]): Program[E, B] =
        MonadError[ZIO[Console with ProgramState, Option[E], *], Option[E]].flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => Program[E, Either[A, B]]): Program[E, B] =
        MonadError[ZIO[Console with ProgramState, Option[E], *], Option[E]].tailRecM(a)(f)

      override def raiseError[A](e: E): Program[E, A] =
        MonadError[ZIO[Console with ProgramState, Option[E], *], Option[E]].raiseError(e.some)

      override def handleErrorWith[A](fa: Program[E, A])(f: E => Program[E, A]): Program[E, A] =
        MonadError[ZIO[Console with ProgramState, Option[E], *], Option[E]].handleErrorWith(fa){
          case Some(e) => f(e)
          case None    => Program.end
        }

      override def pure[A](x: A): Program[E, A] =
        MonadError[ZIO[Console with ProgramState, Option[E], *], Option[E]].pure(x)
    }

    def loadMemory(chunkSize: Int)(fileChannel: AsynchronousFileChannel): ZIO[Console, Exception, List[Long]] =
      fileReadStreamCommaSep(chunkSize, fileChannel)
        .flatMap(strToLong[Stream])
        .runCollect
        .tap(_ => putStrLn("Loading Memory"))

    def runtimeEnv: ZIO[Console with ProgramState, Nothing, Unit] = {
      val result =
        putStrLn("Executing Program")
          .flatMap(_ => CPU.run)
          .optional
          .refineToOrDie[Error]
          .catchAll(Error.asString andThen putStrLn)

      val debug = ProgramState.debug.flatMap(putStrLn)

      result *> debug
    }
  }
}
