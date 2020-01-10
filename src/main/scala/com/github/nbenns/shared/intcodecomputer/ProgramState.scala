package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.intcodecomputer.ValueType.AbsPointer
import zio.{Ref, UIO, ZIO}
import cats.implicits._

trait ProgramState {
  val programState: ProgramState.PrgState
}

object ProgramState {
  trait PrgState {
    val memory: Ref[Memory]
    val cpu: Ref[CPU]
  }

  def bootup(memory: List[Long]): UIO[PrgState] =
    for {
      m  <- Ref.make(Memory(memory ++ (0 to 1023).toList.as(0)))
      c  <- Ref.make(CPU(AbsPointer(0), AbsPointer(0)))
    } yield new PrgState {
      override val memory: Ref[Memory] = m
      override val cpu: Ref[CPU] = c
    }

  def debug: ZIO[ProgramState, Nothing, String] =
    for {
      prgState <- ZIO.access[ProgramState](_.programState)
      mem      <- prgState.memory.get
      cpu      <- prgState.cpu.get
    } yield s"""
         |*** DEBUG ***
         |
         |CPU:
         |  Instruction Pointer: ${cpu.instructionPointer.ref}
         |  Data Pointer:        ${cpu.dataPointer.ref}
         |
         |Memory Dump:
         |${mem.values.mkString(",")}
         |""".stripMargin
}
