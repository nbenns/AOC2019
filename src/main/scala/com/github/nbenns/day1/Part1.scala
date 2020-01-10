package com.github.nbenns.day1

object Part1 extends Function[Long, Long] {
  private val calculateFuelForModule: Long => Long = mass => (mass / 3) - 2;

  override def apply(mass: Long): Long = calculateFuelForModule(mass)
}
