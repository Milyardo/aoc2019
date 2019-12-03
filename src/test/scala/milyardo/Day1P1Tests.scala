package milyardo

object Day1P1Tests {
  def main(args: Array[String]): Unit = {
    val givens = List(12,14,1969,100756)
    val expected = List(2,2,654,33583)

    (givens zip expected).foreach({ case (given, expected) =>
      val actual = Day1P1.launchFuel(given)
      println(s"launchFuel($given) == $actual($expected)")
      assert(expected == actual)
    })
  }
}
