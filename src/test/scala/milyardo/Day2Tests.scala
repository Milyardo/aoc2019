package milyardo

object Day2Tests {
  def main(args: Array[String]): Unit = {
    val ints = Day2.parse("1,9,10,3,2,3,11,0,99,30,40,50")
    println(ints)
    val nextOp = Day2.OpCode.nextOp(ints)

    println(nextOp)

    val ops = Day2.OpCode.parse(ints)
    println(ops.mkString("\n"))
  }
}
