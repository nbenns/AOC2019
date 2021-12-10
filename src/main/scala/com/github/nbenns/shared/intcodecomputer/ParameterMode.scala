package com.github.nbenns.shared.intcodecomputer

import zio.ZIO

sealed trait ParameterMode extends Product with Serializable

object ParameterMode {
  case object PositionMode extends ParameterMode
  case object ImmediateMode extends ParameterMode
  case object ReferenceMode extends ParameterMode

  def fromString(c: Char): ZIO[Any, Option[Instruction.Error], ParameterMode] = c match {
    case '0' => Program.succeed(PositionMode)
    case '1' => Program.succeed(ImmediateMode)
    case '2' => Program.succeed(ReferenceMode)
    case m   => Program.fail(Instruction.Error.InvalidParameterMode(m))
  }
}
