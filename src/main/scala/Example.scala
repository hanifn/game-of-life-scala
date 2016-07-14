class Example {
  def run: String = "Hello, World!"
}

object LiveCellParser   {
  def apply(seed: String): Seq[LiveCell] = {
    seed.split("\n").map { coord =>
      val xy = coord.split(",")

      LiveCell(xy(0).toInt, xy(1).toInt)
    }
  }
}

class LifeGrid(seed: String) {

  val liveSeeds = LiveCellParser(seed)

  def edge = {
    var x = 0
    var y = 0

    for (seed <- liveSeeds) {
      if (seed.x > x) x = seed.x
      if (seed.y > y) y = seed.y
    }

    (x + 2,y + 2)
  }

  def run: String = {
    val (maxX, maxY) = edge

    (0 to maxX).map { x =>
      val row = (0 to maxY).map { y =>
        if (liveSeeds.contains(LiveCell(x - 1, y - 1))) "O"
        else "."
      }

      val joinedRow = row.foldLeft(""){(result,cell) =>  result + cell}
      joinedRow + "\n"

    } mkString ""
  }
}

case class LiveCell(x: Int, y: Int)

object Application extends App {
  val example = new Example
  println(example.run)
}


/**
  * ArrayList<String>
  *  String[]
  */
