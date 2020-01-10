package com.github.nbenns.shared.intcodecomputer

sealed trait Error extends Throwable with Product with Serializable

final case class InvalidOperation(opCode: String, memoryLocation: Int) extends Error

final case class InvalidParameterMode(mode: Char) extends Error

final case class InvalidPointerException(value: Long) extends Error

sealed trait MemoryError extends Error

final case class InvalidMemoryReadLocation(memoryLocation: Int) extends MemoryError

final case class InvalidMemoryWriteLocation(memoryLocation: Int) extends MemoryError

object Error {
  val asString: Error => String = {
    case InvalidOperation(opCode, memoryLocation) =>
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

    case InvalidMemoryReadLocation(memoryLocation) =>
      s"""
         |InvalidMemoryReadLocation:
         |  Read to Invalid Memory Location
         |  Memory Location: $memoryLocation
         |""".stripMargin

    case InvalidMemoryWriteLocation(memoryLocation) =>
      s"""
         |InvalidMemoryWriteLocation:
         |  Write to Invalid Memory Location
         |  Memory Location: $memoryLocation
         |""".stripMargin
  }
}
