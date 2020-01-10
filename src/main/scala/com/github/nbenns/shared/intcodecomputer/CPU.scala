package com.github.nbenns.shared.intcodecomputer

import cats.implicits._
import com.github.nbenns.shared.Conversions._
import com.github.nbenns.shared.intcodecomputer.Program._
import com.github.nbenns.shared.intcodecomputer.ValueType.AbsPointer
import zio.console._

final case class CPU(instructionPointer: AbsPointer, dataPointer: AbsPointer)

object CPU {
  val getCPU: Program[Nothing, CPU] = Program.state.flatMap(_.cpu.get)

  def setCPU(cpu: CPU): Program[Nothing, Unit] =
    Program.state.flatMap(_.cpu.set(cpu))

  def updateCPU(f: CPU => CPU): Program[Nothing, Unit] =
    Program.state.flatMap(_.cpu.modify(c => ((), f(c))))

  val getIP: Program[Nothing, AbsPointer] = getCPU.map(_.instructionPointer)

  def setIP(ipPtr: AbsPointer): Program[Nothing, Unit] =
    getCPU.map(_.copy(instructionPointer = ipPtr)).flatMap(setCPU)

  def updateIP(f: AbsPointer => AbsPointer): Program[Nothing, Unit] =
    updateCPU(cpu => cpu.copy(instructionPointer = f(cpu.instructionPointer)))

  val getDP: Program[Nothing, AbsPointer] = getCPU.map(_.dataPointer)

  def setDP(dpPtr: AbsPointer): Program[Nothing, Unit] =
    getCPU.map(_.copy(dataPointer = dpPtr)).flatMap(setCPU)

  def updateDP(f: AbsPointer => AbsPointer): Program[Nothing, Unit] =
    updateCPU(cpu => cpu.copy(dataPointer = f(cpu.dataPointer)))

  val execute: Instruction => Program[Throwable, Unit] = {
    case Instruction.Add(aVT, bVT, resPointer) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- putStrLn(s" | Add($a, $b, ${resPointer.ref})")
        _ <- ValueType.setValue(resPointer, a + b)
      } yield ()

    case Instruction.Mul(aVT, bVT, resPointer) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- putStrLn(s" | Mul($a, $b, ${resPointer.ref})")
        _ <- ValueType.setValue(resPointer, a * b)
      } yield ()

    case Instruction.Input(resPointer) =>
      for {
        _        <- putStr(" | Input: ")
        input    <- getStrLn.mapError(_.some)
        intInput <- strToLong[Program](input)
        _        <- ValueType.setValue(resPointer, intInput)
      } yield ()

    case Instruction.Output(aPointer) =>
      for {
        output <- ValueType.getValue(aPointer)
        _      <- putStrLn(s" | Output: $output")
      } yield ()

    case Instruction.JumpIfTrue(value, jumpTo) =>
      for {
        v       <- ValueType.getValue(value)
        jt      <- ValueType.getValue(jumpTo)
        jtPtr   <- AbsPointer(jt)
        _       <- putStrLn(s" | JumpIfTrue($v, $jt)")

        ip  <- getIP
        loc  = if (v == 0) ip else jtPtr

        _ <- setIP(loc)
      } yield ()

    case Instruction.JumpIfFalse(value, jumpTo) =>
      for {
        v       <- ValueType.getValue(value)
        jt      <- ValueType.getValue(jumpTo)
        jtPtr   <- AbsPointer(jt)
        _       <- putStrLn(s" | JumpIfFalse($v, $jt)")

        ip  <- getIP
        loc  = if (v == 0) jtPtr else ip

        _ <- setIP(loc)
      } yield ()

    case Instruction.LessThan(aVT, bVT, resPtr) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- putStrLn(s" | LessThan($a, $b, ${resPtr.ref})")

        res  = if (a < b) 1 else 0
        _   <- ValueType.setValue(resPtr, res)
      } yield ()

    case Instruction.Equals(aVT, bVT, resPtr) =>
      for {
        a <- ValueType.getValue(aVT)
        b <- ValueType.getValue(bVT)
        _ <- putStrLn(s" | Equals($a, $b, ${resPtr.ref})")

        res  = if (a == b) 1 else 0
        _   <- ValueType.setValue(resPtr, res)
      } yield ()

    case Instruction.DPAdd(aVT) =>
      for {
        a  <- ValueType.getValue(aVT)
        i  <- longToInt[Program](a)
        _  <- putStrLn(s" | DPAdd($a)")
        dp <- getDP
        ptr =  AbsPointer(dp.ref + i)
        _   <- setDP(ptr)
      } yield ()

    case Instruction.End() => Program.end
  }

  val run: Program[Throwable, Unit] =
    Instruction
      .read
      .flatMap(CPU.execute)
      .forever
}
