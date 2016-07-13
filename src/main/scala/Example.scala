class Example {
  def run: String = "Hello, World!"
}

object Application extends App {
  val example = new Example
  println(example.run)
}
