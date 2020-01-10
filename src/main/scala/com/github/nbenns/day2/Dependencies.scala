package com.github.nbenns.day2

import com.github.nbenns.shared.intcodecomputer.ProgramState
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.scheduler.Scheduler
import zio.system.System

trait Dependencies extends Clock with Scheduler with Console with System with Random with Blocking with ProgramState

object Dependencies {
  def apply(memory: List[Long]): ZIO[ZEnv, Nothing, Dependencies] = for {
    sysClock     <- ZIO.access[Clock](_.clock)
    sysConsole   <- ZIO.access[Console](_.console)
    sysSystem    <- ZIO.access[System](_.system)
    sysRandom    <- ZIO.access[Random](_.random)
    sysBlocking  <- ZIO.access[Blocking](_.blocking)
    prgState     <- ProgramState.bootup(memory)
  } yield new Dependencies {
    override val clock: Clock.Service[Any] = sysClock
    override val system: System.Service[Any] = sysSystem
    override val console: Console.Service[Any] = sysConsole
    override val random: Random.Service[Any] = sysRandom
    override val blocking: Blocking.Service[Any] = sysBlocking
    override val scheduler: Scheduler.Service[Any] = zio.scheduler.SchedulerLive.scheduler
    override val programState: ProgramState.PrgState = prgState
  }
}
