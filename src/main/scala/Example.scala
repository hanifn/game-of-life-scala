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

object LifeGrid {
  def apply(seed: String): LifeGrid = {
    new LifeGrid(LiveCellParser(seed))
  }
}

class LifeGrid(val liveCells: Seq[LiveCell]) {

  def size = {
    var x = 0
    var y = 0

    for (cell <- liveCells) {
      if (cell.x > x) x = cell.x
      if (cell.y > y) y = cell.y
    }

    (x + 2,y + 2)
  }

  def run: String = {
    val (maxX, maxY) = size

    (0 to maxX).map { x =>
      (0 to maxY).map { y =>
        if (liveCells.contains(LiveCell(x - 1, y - 1))) "O"
        else "."
      } mkString("", "", "\n")

    } mkString ""
  }

  def neighbourCount(cell: LiveCell): Int = {
    liveCells.count { c =>
      (Math.abs(c.x - cell.x) == 1) || (Math.abs(c.y - cell.y) == 1)
    }
  }

  def tick: LifeGrid = {
    val (maxX, maxY) = size

    val cellsStillAlive: Seq[LiveCell] = liveCells.filter{ cell =>
      val count = neighbourCount(cell)
      if(count == 2  || count == 3) true
      else false
    }

    new LifeGrid(cellsStillAlive)
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
