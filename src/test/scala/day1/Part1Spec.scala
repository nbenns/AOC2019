package day1

import com.github.nbenns.day1.Part1
import zio.test.Assertion._
import zio.test._

object Part1Spec {
  val test1 = test("For a mass of 12") {
    val fuelForModule = Part1(12)

    assert(fuelForModule)(equalTo(2L))
  }

  val test2 = test("For a mass of 14") {
    val fuelForModule = Part1(14)

    assert(fuelForModule)(equalTo(2L))
  }

  val test3 = test("For a mass of 1969") {
    val fuelForModule = Part1(1969)

    assert(fuelForModule)(equalTo(654L))
  }

  val test4 = test("For a mass of 100756") {
    val fuelForModule = Part1(100756)

    assert(fuelForModule)(equalTo(33583L))
  }

  val testSuite = suite("Part1")(test1, test2, test3, test4)
}


