class Example {
  def run: String = "Hello, World!"
}

object LiveCellParser   {
  def apply(seed: String): Set[LiveCell] = {
    seed.split("\n").map { coord =>
      val xy = coord.split(",")

      LiveCell(xy(0).toInt, xy(1).toInt)
    }.toSet
  }
}

object LifeGrid {
  def apply(seed: String): LifeGrid = {
    new LifeGrid(LiveCellParser(seed))
  }
}

class LifeGrid(val liveCells: Set[LiveCell]) {

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
      val dx = c.x - cell.x
      val dy = c.y - cell.y

      //(Math.abs(c.x - cell.x) < 2) && (Math.abs(c.y - cell.y) < 2 && (c.x != cell.x && c.y != cell.y))
      (dx >= -1 && dx <= 1) &&
      (dy >= -1 && dy <= 1) &&
      (cell != c)


    }
  }

  def getAdjacentCells: Set[LiveCell] =
    liveCells.flatMap{cell =>
      val x = cell.x
      val y = cell.y
      //if (liveCells.contains(LiveCell(cell.x - 1, cell.y)))
      Set(
        LiveCell(x-1, y-1),
        LiveCell(x, y-1),
        LiveCell(x+1, y-1),
        LiveCell(x-1, y),
        LiveCell(x+1, y),
        LiveCell(x-1, y +1),
        LiveCell(x, y+1),
        LiveCell(x+1, y +1)
      )
    }

  def tick: LifeGrid = {
    val (maxX, maxY) = size

    val cellsStillAlive: Set[LiveCell] = liveCells.filter{ cell =>
      val count = neighbourCount(cell)
      if(count == 2  || count == 3) true
      else false
    }

    val adjacentCells: Set[LiveCell] = getAdjacentCells

    val deadCells = adjacentCells.filterNot{ cell =>
      liveCells.contains(cell)
    }

    val newLiveCells = deadCells.filter{ cell =>
      neighbourCount(cell) == 3
    }

    new LifeGrid(cellsStillAlive ++ newLiveCells)
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
