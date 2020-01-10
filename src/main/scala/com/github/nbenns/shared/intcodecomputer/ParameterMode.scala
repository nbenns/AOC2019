package com.github.nbenns.shared.intcodecomputer

sealed trait ParameterMode extends Product with Serializable

object ParameterMode {
  final case object PositionMode extends ParameterMode
  final case object ImmediateMode extends ParameterMode
  final case object ReferenceMode extends ParameterMode

  def fromString(c: Char): Program[InvalidParameterMode, ParameterMode] = c match {
    case '0' => Program.succeed(PositionMode)
    case '1' => Program.succeed(ImmediateMode)
    case '2' => Program.succeed(ReferenceMode)
    case m   => Program.fail(InvalidParameterMode(m))
  }
}
