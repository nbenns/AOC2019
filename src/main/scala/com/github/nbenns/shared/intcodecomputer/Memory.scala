package com.github.nbenns.shared.intcodecomputer

case class Memory(values: List[Long])

object Memory {
  def getMemory: Program[Nothing, Memory] = Program.state.flatMap(_.memory.get)

  def setMemory(mem: Memory): Program[Nothing, Unit] =
    Program.state.flatMap(_.memory.set(mem))

  def updateMemory(f: Memory => Memory): Program[Nothing, Unit] =
    Program.state.flatMap(_.memory.modify(m => ((), f(m))))

  def read(n: Int): Program[InvalidMemoryReadLocation, Long] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.values.length) Program.succeed(mem.values(n))
      else Program.fail(InvalidMemoryReadLocation(n))
    }

  def write(n: Int, v: Long): Program[InvalidMemoryWriteLocation, Unit] =
    getMemory.flatMap { mem =>
      if (n >= 0 && n < mem.values.length) setMemory(mem.copy(values = mem.values.updated(n, v)))
      else Program.fail(InvalidMemoryWriteLocation(n))
    }
}
