import org.scalatest.{ FunSuite, MustMatchers, WordSpec }

class LifeGridTest extends WordSpec with MustMatchers {


  //TODO: Maybe move the test down to the parser?
  "Companion Object" must {

    "construct a LifeGrid with the seed data" in {
      val seed =
        """0,0
          |0,1
          |1,0
          |1,1""".stripMargin

      val grid = LifeGrid(seed)

      grid.liveCells mustEqual Set(LiveCell(0, 0), LiveCell(0, 1), LiveCell(1, 0), LiveCell(1, 1))
    }
  }

  "run" must {
    "return a board representation for a block" in {
      val seed = "0,0\n0,1\n1,0\n1,1"
      val grid = LifeGrid(seed)

      val result = grid.run

      result mustEqual "....\n.OO.\n.OO.\n....\n"
    }

    "return a board representation for a boat" in {
      val seed =
        """0,0
          |0,1
          |1,0
          |2,1
          |1,2
          |""".stripMargin
      val grid = LifeGrid(seed)

      val result = grid.run

      result mustEqual
        """.....
          |.OO..
          |.O.O.
          |..O..
          |.....
          |""".stripMargin
    }
  }

  "size" must {

    "find the size of the grid" in {
      val seed =
        """0,0
          |0,1
          |1,0
          |2,1
          |1,2
          |""".stripMargin

      val grid = LifeGrid(seed)

      val result = grid.size

      result mustEqual (4,4)
    }
  }

  "neigbourCount" must {

    "find the count of live neigbhours of a given cell" in {
      val grid = new LifeGrid(Set(LiveCell(0,0), LiveCell(0,1), LiveCell(0,2)))

      grid.neighbourCount(LiveCell(0,0)) mustEqual 1
      grid.neighbourCount(LiveCell(0,1)) mustEqual 2
      grid.neighbourCount(LiveCell(0,2)) mustEqual 1

      grid.neighbourCount(LiveCell(-1,-1)) mustEqual 1
    }
  }

  "getAdjacentCell" must {

    "return all neighbours of cell" in {
      val grid = new LifeGrid(Set(LiveCell(0,0)))

      grid.getAdjacentCells mustEqual Set(
        LiveCell(-1,-1),
        LiveCell(-1,0),
        LiveCell(-1,1),
        LiveCell(0,-1),
        LiveCell(0,1),
        LiveCell(+1,-1),
        LiveCell(+1,0),
        LiveCell(+1,1)
      )
    }
  }

  "tick" must {
    "iterate to next grid state" in {
      val seed =
        """0,0
          |1,0
          |2,0
          |""".stripMargin

      val grid = LifeGrid(seed)

      val nextGrid = grid.tick

      nextGrid.liveCells mustEqual Set(LiveCell(1,-1), LiveCell(1,0), LiveCell(1,1))
    }
  }

}
