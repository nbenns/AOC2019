package com.github.nbenns.shared.intcodecomputer

import zio.*

class Memory(memory: Ref[List[Long]]) {
  def getMemory: ZIO[Any, Nothing, List[Long]] = memory.get
  def setMemory(mem: List[Long]): ZIO[Any, Nothing, Unit] = memory.set(mem)
  def updateMemory(f: List[Long] => List[Long]): ZIO[Any, Nothing, Unit] = memory.update(f)
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

  def live(initial: List[Long]): ZLayer[Any, Nothing, Memory] =
    Ref.make(initial ++ (0 to 1023).toList.map(_ => 0L))
      .map(new Memory(_))
      .toLayer

  def getMemory: ZIO[Memory, Nothing, List[Long]] =
    ZIO.serviceWithZIO(_.getMemory)

  def setMemory(mem: List[Long]): ZIO[Memory, Nothing, Unit] =
    ZIO.serviceWithZIO(_.setMemory(mem))

  def updateMemory(f: List[Long] => List[Long]): ZIO[Memory, Nothing, Unit] =
    ZIO.serviceWithZIO(_.updateMemory(f))

  def read(n: Int): ZIO[Memory, Option[Error], Long] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.length) Program.succeed(mem(n))
      else Program.fail(Error.InvalidMemoryReadLocation(n))
    }

  def write(n: Int, v: Long): ZIO[Memory, Option[Error], Unit] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.length) setMemory(mem.updated(n, v))
      else Program.fail(Error.InvalidMemoryWriteLocation(n))
    }
}
