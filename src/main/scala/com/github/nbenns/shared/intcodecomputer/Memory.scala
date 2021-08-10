package com.github.nbenns.shared.intcodecomputer

import zio.{Has, Ref, ZIO, ZLayer}

class Memory(memory: Ref[List[Long]]) {
  def getMemory: IProgram[Nothing, List[Long]] = memory.get
  def setMemory(mem: List[Long]): IProgram[Nothing, Unit] = memory.set(mem)
  def updateMemory(f: List[Long] => List[Long]): IProgram[Nothing, Unit] = memory.update(f)
}

object Memory {
  enum Error extends Throwable {
    case InvalidMemoryReadLocation(memoryLocation: Int)
    case InvalidMemoryWriteLocation(memoryLocation: Int)

    extension (self: Error) {
      def asString: String = self match {
        case InvalidMemoryReadLocation(memoryLocation) =>
          s"""
             |InvalidMemoryReadLocation:
             |  Read to Invalid Memory Location
             |  Memory Location: $memoryLocation
             |""".stripMargin

        case InvalidMemoryWriteLocation(memoryLocation) =>
          s"""
             |InvalidMemoryWriteLocation:
             |  Write to Invalid Memory Location
             |  Memory Location: $memoryLocation
             |""".stripMargin
      }
    }
  }

  def live(initial: List[Long]): ZLayer[Any, Nothing, Has[Memory]] =
    Ref.make(initial ++ (0 to 1023).toList.map(_ => 0L))
      .map(new Memory(_))
      .toLayer

  def getMemory: RProgram[Has[Memory], Nothing, List[Long]] =
    ZIO.serviceWith(_.getMemory)

  def setMemory(mem: List[Long]): RProgram[Has[Memory], Nothing, Unit] =
    ZIO.serviceWith(_.setMemory(mem))

  def updateMemory(f: List[Long] => List[Long]): RProgram[Has[Memory], Nothing, Unit] =
    ZIO.serviceWith(_.updateMemory(f))

  def read(n: Int): RProgram[Has[Memory], Error, Long] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.length) Program.succeed(mem(n))
      else Program.fail(Error.InvalidMemoryReadLocation(n))
    }

  def write(n: Int, v: Long): RProgram[Has[Memory], Error, Unit] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.length) setMemory(mem.updated(n, v))
      else Program.fail(Error.InvalidMemoryWriteLocation(n))
    }
}
