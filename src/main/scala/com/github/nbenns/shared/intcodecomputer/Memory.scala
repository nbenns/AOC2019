package com.github.nbenns.shared.intcodecomputer

import zio.{Has, Ref, ZIO, ZLayer}
import cats.implicits._

object Memory {
  type Memory = Has[Memory.Service]

  class Service(memory: Ref[List[Long]]) {
    def getMemory: IProgram[Nothing, List[Long]] = memory.get
    def setMemory(mem: List[Long]): IProgram[Nothing, Unit] = memory.set(mem)
    def updateMemory(f: List[Long] => List[Long]): IProgram[Nothing, Unit] = memory.update(f)
  }

  def live(initial: List[Long]): ZLayer[Any, Nothing, Has[Service]] =
    Ref.make(initial ++ (0 to 1023).toList.as(0L))
      .map(new Service(_))
      .toLayer

  def getMemory: RProgram[Memory, Nothing, List[Long]] = ZIO.accessM(_.get.getMemory)

  def setMemory(mem: List[Long]): RProgram[Memory, Nothing, Unit] = ZIO.accessM(_.get.setMemory(mem))

  def updateMemory(f: List[Long] => List[Long]): RProgram[Memory, Nothing, Unit] =
    ZIO.accessM(_.get.updateMemory(f))

  def read(n: Int): RProgram[Memory, InvalidMemoryReadLocation, Long] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.length) Program.succeed(mem(n))
      else Program.fail(InvalidMemoryReadLocation(n))
    }

  def write(n: Int, v: Long): RProgram[Memory, InvalidMemoryWriteLocation, Unit] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.length) setMemory(mem.updated(n, v))
      else Program.fail(InvalidMemoryWriteLocation(n))
    }
}
