import zio.test._

object All extends DefaultRunnableSpec {
  val spec = suite("All Tests")(
    day1.testSuite
  )
}
