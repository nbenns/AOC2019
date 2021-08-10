package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.Conversions.{longToInt, strToLong}
import com.github.nbenns.shared.intcodecomputer.CPU.{getDP, getIP, setDP, setIP}
import com.github.nbenns.shared.intcodecomputer.ValueType.AbsPointer
import zio.Console.*

trait Executer {
  val execute: Instruction => Program[Throwable, Unit] = {
    case Instruction.Add(aVT, bVT, resPointer) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- printLine(s" | Add($a, $b, ${resPointer.ref})").asSomeError
        _ <- ValueType.setValue(resPointer, a + b)
      } yield ()

    case Instruction.Mul(aVT, bVT, resPointer) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- printLine(s" | Mul($a, $b, ${resPointer.ref})").asSomeError
        _ <- ValueType.setValue(resPointer, a * b)
      } yield ()

    case Instruction.Input(resPointer) =>
      for {
        _        <- print(" | Input: ").asSomeError
        input    <- readLine.asSomeError
        intInput <- Program.fromEither(strToLong(input))
        _        <- ValueType.setValue(resPointer, intInput)
      } yield ()

    case Instruction.Output(aPointer) =>
      for {
        output <- ValueType.getValue(aPointer)
        _      <- printLine(s" | Output: $output").asSomeError
      } yield ()

    case Instruction.JumpIfTrue(value, jumpTo) =>
      for {
        v       <- ValueType.getValue(value)
        jt      <- ValueType.getValue(jumpTo)
        jtPtr   <- AbsPointer(jt)
        _       <- printLine(s" | JumpIfTrue($v, $jt)").asSomeError

        ip  <- getIP
        loc  = if (v == 0) ip else jtPtr

        _ <- setIP(loc)
      } yield ()

    case Instruction.JumpIfFalse(value, jumpTo) =>
      for {
        v       <- ValueType.getValue(value)
        jt      <- ValueType.getValue(jumpTo)
        jtPtr   <- AbsPointer(jt)
        _       <- printLine(s" | JumpIfFalse($v, $jt)").asSomeError

        ip  <- getIP
        loc  = if (v == 0) jtPtr else ip

        _ <- setIP(loc)
      } yield ()

    case Instruction.LessThan(aVT, bVT, resPtr) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- printLine(s" | LessThan($a, $b, ${resPtr.ref})").asSomeError

        res  = if (a < b) 1 else 0
        _   <- ValueType.setValue(resPtr, res)
      } yield ()

    case Instruction.Equals(aVT, bVT, resPtr) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- printLine(s" | Equals($a, $b, ${resPtr.ref})").asSomeError

        res  = if (a == b) 1 else 0
        _   <- ValueType.setValue(resPtr, res)
      } yield ()

    case Instruction.DPAdd(aVT) =>
      for {
        a  <- ValueType.getValue(aVT)
        i  <- Program.fromEither(longToInt(a))
        _  <- printLine(s" | DPAdd($a)").asSomeError
        dp <- getDP
        ptr =  AbsPointer(dp.ref + i)
        _   <- setDP(ptr)
      } yield ()

    case Instruction.End => Program.end
  }
}
