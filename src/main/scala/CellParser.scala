/**
  * Created by hanifnorman on 14/7/16.
  */
object CellParser {
  def apply(seed: String): Set[Cell] = {
    seed.split("\n").map { coord =>
      val xy = coord.split(",")

      Cell(xy(0).toInt, xy(1).toInt)
    }.toSet
  }
}
