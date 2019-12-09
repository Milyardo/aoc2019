package milyardo

object Day3Tests {
  val input = "R8,U5,L5,D3\nU7,R6,D4,L4"
  def main(args: Array[String]): Unit = {
    val wires = Day3.parse(input)
    println(wires)
    println(Day3.bounds(wires))
    println(Day3.draw(wires))
    val coords: List[List[(Int, Int)]] = wires.map(Day3.coords)
    val intersects                     = coords.map(_.toSet).reduce(_ intersect _)
    println(coords)
    println(intersects)
  }
}
