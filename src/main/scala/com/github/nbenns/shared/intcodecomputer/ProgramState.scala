package com.github.nbenns.shared.intcodecomputer

import com.github.nbenns.shared.intcodecomputer.CPU.{getDP, getIP}
import com.github.nbenns.shared.intcodecomputer.Memory.getMemory
import zio.*

object ProgramState {
  def deps(initialMemory: List[Long]): ZLayer[Any, Nothing, CPU & Memory] =
    CPU.live ++ Memory.live(initialMemory)

  def debug: ZIO[CPU & Memory, Option[Nothing], String] =
    for {
      mem      <- getMemory
      ip       <- getIP
      dp       <- getDP
    } yield s"""
         |*** DEBUG ***
         |
         |CPU:
         |  Instruction Pointer: ${ip}
         |  Data Pointer:        ${dp}
         |
         |Memory Dump:
         |${mem.mkString(",")}
         |""".stripMargin
}
