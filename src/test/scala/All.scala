import zio.test._

object All extends DefaultRunnableSpec(
  suite("All Tests")(
    day1.testSuite
  )
)
