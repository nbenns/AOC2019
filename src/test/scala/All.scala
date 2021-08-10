import zio.test.*

object All extends DefaultRunnableSpec {
  val spec = suite("All Tests")(
    day1.testSuite
  )
}
