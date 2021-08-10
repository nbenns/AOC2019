package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.Conversions.*
import zio.Has

sealed trait ValueType extends Product with Serializable

object ValueType {
  abstract case class Value private(value: Long) extends ValueType
  object Value {
    def apply(l: Long): Value = new Value(l) {}
  }

  abstract case class AbsPointer private(ref: Int) extends ValueType
  object AbsPointer {
    def apply(l: Long): IProgram[Instruction.Error, AbsPointer] =
      Program
        .fromEither(longToInt(l))
        .mapBoth(
          _.map(_ => Instruction.Error.InvalidPointerException(l)),
          i => new AbsPointer(i) {}
        )

    def apply(i: Int): AbsPointer = new AbsPointer(i) {}
  }

  abstract case class RefPointer private(ref: Int) extends ValueType
  object RefPointer {
    def apply(l: Long): IProgram[Instruction.Error, RefPointer] =
      Program
        .fromEither(longToInt(l))
        .mapBoth(
          _.map(_ => Instruction.Error.InvalidPointerException(l)),
          i => new RefPointer(i) {}
        )
  }

  val getValue: ValueType => RProgram[Has[CPU] & Has[Memory], Memory.Error, Long] = {
    case Value(i)      => Program.succeed(i)
    case AbsPointer(i) => Memory.read(i)
    case RefPointer(i) => CPU.getDP.map(_.ref + i).flatMap(Memory.read)
  }

  def setValue(pointer: AbsPointer, value: Long): RProgram[Has[Memory], Memory.Error, Unit] =
    Memory.write(pointer.ref, value)

  def incPointer(pointer: AbsPointer): AbsPointer = AbsPointer(pointer.ref + 1)
}
