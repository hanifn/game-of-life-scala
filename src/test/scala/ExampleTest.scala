import org.scalatest.{ MustMatchers, WordSpec }

class ExampleTest extends WordSpec with MustMatchers {

  "run" must {

    "return Hello World" in {
      val subject = new Example()

      val result = subject.run

      result mustEqual "Hello, World!"
    }
  }
}
