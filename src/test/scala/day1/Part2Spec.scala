package day1

import com.github.nbenns.day1.Part2
import zio.test.Assertion.equalTo
import zio.test.*

object Part2Spec {
  val test1 = test("For a mass of 14") {
    val fuelForModule = Part2(14)

    assert(fuelForModule)(equalTo(2L))
  }

  val test2 = test("For a mass of 1969") {
    val fuelForModule = Part2(1969)

    assert(fuelForModule)(equalTo(966L))
  }

  val test3 = test("For a mass of 100756") {
    val fuelForModule = Part2(100756)

    assert(fuelForModule)(equalTo(50346L))
  }

  val testSuite = suite("Part2")(test1, test2, test3)
}
