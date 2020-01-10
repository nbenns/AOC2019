package com.github.nbenns.day1

object Part2 extends Function[Long, Long] {
  private val calculateFuelForFuel: Long => Long = fuel =>
    if (Part1(fuel) <= 0) fuel
    else calculateFuelForFuel(Part1(fuel)) + fuel

  override def apply(mass: Long): Long = (Part1 andThen calculateFuelForFuel)(mass)
}
