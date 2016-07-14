import org.scalatest.{ FunSuite, MustMatchers, WordSpec }

class LifeGridTest extends WordSpec with MustMatchers {


  "constructor" must {

    "store a list representation of the seed data" in {
      val seed =
        """0,0
          |0,1
          |1,0
          |1,1""".stripMargin

      val grid = new LifeGrid(seed)

      grid.liveSeeds mustEqual Seq(LiveCell(0, 0), LiveCell(0, 1), LiveCell(1, 0), LiveCell(1, 1))

    }
  }

  "run" must {
    "return a board representation " in {
      val seed = "0,0\n0,1\n1,0\n1,1"
      val grid = new LifeGrid(seed)

      val result = grid.run

      result mustEqual "....\n.OO.\n.OO.\n....\n"
    }
  }

  "edge" must {

    "find the extreme edge of live cells" in {
      val seed =
        """0,0
          |0,1
          |1,0
          |2,1
          |1,2
          |""".stripMargin

      val grid = new LifeGrid(seed)

      val result = grid.edge

      result mustEqual (4,4)
    }
  }
}
