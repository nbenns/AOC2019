package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.Conversions.*
import zio.Console.*
import zio.{Has, ZIO}

enum Instruction {
  case Add(a: ValueType, b: ValueType, res: ValueType.AbsPointer)
  case Mul(a: ValueType, b: ValueType, res: ValueType.AbsPointer)
  case Input(loc: ValueType.AbsPointer)
  case Output(value: ValueType)
  case JumpIfTrue(value: ValueType, jumpTo: ValueType)
  case JumpIfFalse(value: ValueType, jumpTo: ValueType)
  case LessThan(a: ValueType, b: ValueType, res: ValueType.AbsPointer)
  case Equals(a: ValueType, b: ValueType, res: ValueType.AbsPointer)
  case DPAdd(a: ValueType)
  case End
}

object Instruction {
  enum Error extends Throwable {
    case InvalidOpCode(opCode: String, memoryLocation: Int)
    case InvalidParameterMode(mode: Char)
    case InvalidPointerException(value: Long)

    extension (self: Error) {
      def asString: String = self match {
        case InvalidOpCode(opCode, memoryLocation) =>
          s"""
             |InvalidOperation:
             |  Invalid OpCode:  $opCode
             |  Memory Location: $memoryLocation
             |""".stripMargin

        case InvalidParameterMode(mode) =>
          s"""
             |InvalidParameterMode:
             |  Mode:  $mode
             |""".stripMargin

        case InvalidPointerException(value) =>
          s"""
             |InvalidPointerException:
             |  Value:  $value
             |""".stripMargin
      }
    }
  }

  private val readAndNext: RProgram[Has[CPU] & Has[Memory], Memory.Error, Long] =
    for {
      ip <- CPU.getIP
      v  <- ValueType.getValue(ip)
      _  <- CPU.updateIP(ValueType.incPointer)
    } yield v

  private def buildParameter(pm: ParameterMode)(v: Long): IProgram[Instruction.Error, ValueType] =
    pm match {
      case ParameterMode.PositionMode => ValueType.AbsPointer(v)
      case ParameterMode.ImmediateMode => Program.succeed(ValueType.Value(v))
      case ParameterMode.ReferenceMode => ValueType.RefPointer(v)
    }

  private def getParamModeForParam(n: Int, parameterModes: List[ParameterMode]): ParameterMode =
    parameterModes
      .slice(n - 1, n)
      .headOption
      .getOrElse(ParameterMode.PositionMode)

  private def buildAdd(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes))

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Add(aPointer, bPointer, resPointer)
  }

  private def buildMul(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes))

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Mul(aPointer, bPointer, resPointer)
  }

  private def buildInput(): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Input] =
    for {
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Input(resPointer)

  private def buildOutput(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))

    for {
      aPointer <- readAndNext.flatMap(buildParam1)
    } yield Output(aPointer)
  }

  private def buildJumpIfTrue(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes))

    for {
      value  <- readAndNext.flatMap(buildParam1)
      jumpTo <- readAndNext.flatMap(buildParam2)
    } yield JumpIfTrue(value, jumpTo)
  }

  private def buildJumpIfFalse(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes))

    for {
      value  <- readAndNext.flatMap(buildParam1)
      jumpTo <- readAndNext.flatMap(buildParam2)
    } yield JumpIfFalse(value, jumpTo)
  }

  private def buildLessThan(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes))

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield LessThan(aPointer, bPointer, resPointer)
  }

  private def buildEquals(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))
    val buildParam2 = buildParameter(getParamModeForParam(2, parameterModes))

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
      bPointer   <- readAndNext.flatMap(buildParam2)
      resPointer <- readAndNext.flatMap(ValueType.AbsPointer.apply)
    } yield Equals(aPointer, bPointer, resPointer)
  }

  private def buildDPAdd(parameterModes: List[ParameterMode]): RProgram[Has[CPU] & Has[Memory], Instruction.Error | Memory.Error, Instruction] = {
    val buildParam1 = buildParameter(getParamModeForParam(1, parameterModes))

    for {
      aPointer   <- readAndNext.flatMap(buildParam1)
    } yield DPAdd(aPointer)
  }

  private def buildEnd: RProgram[Any, Nothing, Instruction] = Program.succeed(End)

  def read: Program[Instruction.Error | Memory.Error, Instruction] =
    for {
      ip        <- CPU.getIP
      opCode    <- readAndNext
      opCodeStr  = opCode.toString

      inst           <- Program.fromEither(strToInt(opCodeStr.takeRight(2)))
        .orElseFail(Some(Error.InvalidOpCode(opCodeStr.takeRight(2), ip.ref)))
      paramList      =  opCodeStr.dropRight(2).reverse
      parameterModes <- ZIO.foreach(paramList.toList)(ParameterMode.fromString)

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
        case _  => Program.fail(Error.InvalidOpCode(opCodeStr, ip.ref))
      }

      _ <- print(s"${ip.ref}: $operation").ignore
    } yield operation
}
