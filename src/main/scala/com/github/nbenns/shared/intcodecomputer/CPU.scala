package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.Conversions.*
import com.github.nbenns.shared.intcodecomputer.ValueType.AbsPointer
import zio.*
import zio.Console.*

trait CPU {
  def getIP: IProgram[Nothing, AbsPointer]
  def setIP(ipPtr: AbsPointer): IProgram[Nothing, Unit]
  def updateIP(f: AbsPointer => AbsPointer): IProgram[Nothing, Unit]

  def getDP: IProgram[Nothing, AbsPointer]
  def setDP(dpPtr: AbsPointer): IProgram[Nothing, Unit]
  def updateDP(f: AbsPointer => AbsPointer): IProgram[Nothing, Unit]
}

object CPU {
  private case class Live(ip: Ref[AbsPointer], dp: Ref[AbsPointer]) extends CPU {
    def getIP: IProgram[Nothing, AbsPointer] = ip.get
    def setIP(ipPtr: AbsPointer): IProgram[Nothing, Unit] = ip.set(ipPtr)
    def updateIP(f: AbsPointer => AbsPointer): IProgram[Nothing, Unit] = ip.update(f)

    def getDP: IProgram[Nothing, AbsPointer] = dp.get
    def setDP(dpPtr: AbsPointer): IProgram[Nothing, Unit] = dp.set(dpPtr)
    def updateDP(f: AbsPointer => AbsPointer): IProgram[Nothing, Unit] = dp.update(f)
  }

  val live: ZLayer[Any, Nothing, Has[CPU]] =
    Ref
      .make(AbsPointer(0))
      .zip(Ref.make(AbsPointer(0)))
      .map(Live.apply)
      .toLayer

  val getIP: RProgram[Has[CPU], Nothing, AbsPointer] =
    ZIO.serviceWith(_.getIP)

  def setIP(ipPtr: AbsPointer): RProgram[Has[CPU], Nothing, Unit] =
    ZIO.serviceWith(_.setIP(ipPtr))

  def updateIP(f: AbsPointer => AbsPointer): RProgram[Has[CPU], Nothing, Unit] =
    ZIO.serviceWith(_.updateIP(f))

  val getDP: RProgram[Has[CPU], Nothing, AbsPointer] =
    ZIO.serviceWith(_.getDP)

  def setDP(dpPtr: AbsPointer): RProgram[Has[CPU], Nothing, Unit] =
    ZIO.serviceWith(_.setDP(dpPtr))

  def updateDP(f: AbsPointer => AbsPointer): RProgram[Has[CPU], Nothing, Unit] =
    ZIO.serviceWith(_.updateDP(f))

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

  val run: Program[Throwable, Unit] =
    Instruction
      .read
      .flatMap((new Executer {}).execute)
      .forever
}
