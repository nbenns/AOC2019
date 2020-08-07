package com.github.nbenns.shared.intcodecomputer

import cats.implicits._
import com.github.nbenns.shared.Conversions._
import com.github.nbenns.shared.intcodecomputer.CPU.CPU
import com.github.nbenns.shared.intcodecomputer.Memory.Memory
import com.github.nbenns.shared.intcodecomputer.Program._

sealed trait ValueType extends Product with Serializable

object ValueType {
  abstract case class Value private(value: Long) extends ValueType
  object Value {
    def apply(l: Long): Value = new Value(l) {}
  }

  abstract case class AbsPointer private(ref: Int) extends ValueType
  object AbsPointer {
    def apply(l: Long): IProgram[InvalidPointerException, AbsPointer] =
      longToInt[IProgram](l).bimap(
        _.as(InvalidPointerException(l)),
        i => new AbsPointer(i) {}
      )

    def apply(i: Int): AbsPointer = new AbsPointer(i) {}
  }

  abstract case class RefPointer private(ref: Int) extends ValueType
  object RefPointer {
    def apply(l: Long): IProgram[InvalidPointerException, RefPointer] =
      longToInt[IProgram](l).bimap(
        _.as(InvalidPointerException(l)),
        i => new RefPointer(i) {}
      )
  }

  val getValue: ValueType => RProgram[CPU with Memory, InvalidMemoryReadLocation, Long] = {
    case Value(i)      => Program.succeed(i)
    case AbsPointer(i) => Memory.read(i)
    case RefPointer(i) => CPU.getDP.map(_.ref + i).flatMap(Memory.read)
  }

  def setValue(pointer: AbsPointer, value: Long): RProgram[Memory, InvalidMemoryWriteLocation, Unit] =
    Memory.write(pointer.ref, value)

  def incPointer(pointer: AbsPointer): AbsPointer = AbsPointer(pointer.ref + 1)
}
