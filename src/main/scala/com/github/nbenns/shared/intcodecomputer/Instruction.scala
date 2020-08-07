package com.github.nbenns.shared.intcodecomputer

import cats.implicits._
import zio.console.putStr
import com.github.nbenns.shared.Conversions._
import Program._
import com.github.nbenns.shared.intcodecomputer.CPU.CPU
import com.github.nbenns.shared.intcodecomputer.Memory.Memory

sealed trait Instruction extends Product with Serializable

object Instruction {
  final case class Add(a: ValueType, b: ValueType, res: ValueType.AbsPointer) extends Instruction
  final case class Mul(a: ValueType, b: ValueType, res: ValueType.AbsPointer) extends Instruction
  final case class Input(loc: ValueType.AbsPointer) extends Instruction
  final case class Output(value: ValueType) extends Instruction
  final case class JumpIfTrue(value: ValueType, jumpTo: ValueType) extends Instruction
  final case class JumpIfFalse(value: ValueType, jumpTo: ValueType) extends Instruction
  final case class LessThan(a: ValueType, b: ValueType, res: ValueType.AbsPointer) extends Instruction
  final case class Equals(a: ValueType, b: ValueType, res: ValueType.AbsPointer) extends Instruction
  final case class DPAdd(a: ValueType) extends Instruction
  final case class End() extends Instruction

  private val readAndNext: RProgram[CPU with Memory, InvalidMemoryReadLocation, Long] =
    for {
      ip <- CPU.getIP
      v  <- ValueType.getValue(ip)
      _  <- CPU.updateIP(ValueType.incPointer)
    } yield v

  private def buildParameter(pm: ParameterMode)(v: Long): IProgram[InvalidPointerException, ValueType] = pm match {
    case ParameterMode.PositionMode  => ValueType.AbsPointer(v)
    case ParameterMode.ImmediateMode => Program.succeed(ValueType.Value(v))
    case ParameterMode.ReferenceMode => ValueType.RefPointer(v)
  }

  private def getParamModeForParam(n: Int, parameterModes: List[ParameterMode]): ParameterMode =
    parameterModes
      .slice(n - 1, n)
      .headOption
      .getOrElse(ParameterMode.PositionMode)

  private def buildAdd(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, Add] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes)) _

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Add(aPointer, bPointer, resPointer)
  }

  private def buildMul(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, Mul] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes)) _

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Mul(aPointer, bPointer, resPointer)
  }

  private def buildInput(): RProgram[CPU with Memory, Error, Input] =
    for {
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Input(resPointer)

  private def buildOutput(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, Output] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _

    for {
      aPointer <- readAndNext.flatMap(buildParam1)
    } yield Output(aPointer)
  }

  private def buildJumpIfTrue(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, JumpIfTrue] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes)) _

    for {
      value  <- readAndNext.flatMap(buildParam1)
      jumpTo <- readAndNext.flatMap(buildParam2)
    } yield JumpIfTrue(value, jumpTo)
  }

  private def buildJumpIfFalse(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, JumpIfFalse] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes)) _

    for {
      value  <- readAndNext.flatMap(buildParam1)
      jumpTo <- readAndNext.flatMap(buildParam2)
    } yield JumpIfFalse(value, jumpTo)
  }

  private def buildLessThan(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, LessThan] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes)) _

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield LessThan(aPointer, bPointer, resPointer)
  }

  private def buildEquals(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, Equals] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes)) _

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Equals(aPointer, bPointer, resPointer)
  }

  private def buildDPAdd(parameterModes: List[ParameterMode]): RProgram[CPU with Memory, Error, DPAdd] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes)) _

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
    } yield DPAdd(aPointer)
  }

  private def buildEnd: RProgram[Any, Nothing, End] = Program.succeed(End())

  def read: Program[Error, Instruction] =
    for {
      ip        <- CPU.getIP
      opCode    <- readAndNext
      opCodeStr  = opCode.toString

      inst           <- strToInt[Program](opCodeStr.takeRight(2))
                          .orElseFail(InvalidOperation(opCodeStr.takeRight(2), ip.ref).some)
      paramList      =  opCodeStr.dropRight(2).reverse
      parameterModes <- paramList.toList.map(ParameterMode.fromString).sequence

      operation <- inst match {
        case 1  => buildAdd(parameterModes)
        case 2  => buildMul(parameterModes)
        case 3  => buildInput()
        case 4  => buildOutput(parameterModes)
        case 5  => buildJumpIfTrue(parameterModes)
        case 6  => buildJumpIfFalse(parameterModes)
        case 7  => buildLessThan(parameterModes)
        case 8  => buildEquals(parameterModes)
        case 9  => buildDPAdd(parameterModes)
        case 99 => buildEnd
        case _  => Program.fail(InvalidOperation(opCodeStr, ip.ref))
      }

      _ <- putStr(s"${ip.ref}: $operation")
    } yield operation
}
