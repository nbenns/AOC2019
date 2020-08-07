package com.github.nbenns.shared.intcodecomputer

import cats.implicits._
import com.github.nbenns.shared.Conversions._
import com.github.nbenns.shared.intcodecomputer.Program._
import com.github.nbenns.shared.intcodecomputer.ValueType.AbsPointer
import zio.{Has, Ref, ZIO, ZLayer}
import zio.console._

object CPU {
  type CPU = Has[CPU.Service]

  class Service(ip: Ref[AbsPointer], dp: Ref[AbsPointer]) {
    def getIP: IProgram[Nothing, AbsPointer] = ip.get
    def setIP(ipPtr: AbsPointer): IProgram[Nothing, Unit] = ip.set(ipPtr)
    def updateIP(f: AbsPointer => AbsPointer): IProgram[Nothing, Unit] = ip.update(f)

    def getDP: IProgram[Nothing, AbsPointer] = dp.get
    def setDP(dpPtr: AbsPointer): IProgram[Nothing, Unit] = dp.set(dpPtr)
    def updateDP(f: AbsPointer => AbsPointer): IProgram[Nothing, Unit] = dp.update(f)
  }

  val live: ZLayer[Any, Nothing, Has[Service]] =
    Ref.make(AbsPointer(0))
      .zip(Ref.make(AbsPointer(0)))
      .map { case (ip, dp) => new Service(ip, dp) }.toLayer

  val getIP: RProgram[CPU, Nothing, AbsPointer] = ZIO.accessM(_.get.getIP)

  def setIP(ipPtr: AbsPointer): RProgram[CPU, Nothing, Unit] = ZIO.accessM(_.get.setIP(ipPtr))

  def updateIP(f: AbsPointer => AbsPointer): RProgram[CPU, Nothing, Unit] =
    ZIO.accessM(_.get.updateIP(f))

  val getDP: RProgram[CPU, Nothing, AbsPointer] = ZIO.accessM(_.get.getDP)

  def setDP(dpPtr: AbsPointer): RProgram[CPU, Nothing, Unit] = ZIO.accessM(_.get.setDP(dpPtr))

  def updateDP(f: AbsPointer => AbsPointer): RProgram[CPU, Nothing, Unit] =
    ZIO.accessM(_.get.updateDP(f))

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
