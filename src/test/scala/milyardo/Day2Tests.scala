package milyardo

import milyardo.Day2.Machine

object Day2Tests {
  def main(args: Array[String]): Unit = {
    val ints = Day2.parse("1,9,10,3,2,3,11,0,99,30,40,50")
    println(ints)
    val nextOp = Day2.OpCode.nextOp(ints)
    println(nextOp)
    val machine1 = Machine(0,ints)
    println(Day2.run(machine1))
    val machine2 = Machine(0,List(1,0,0,0,99))
    println(Day2.run(machine2))
    val machine3 = Machine(0,List(2,3,0,3,99))
    println(Day2.run(machine3))
    val machine4 = Machine(0,List(2,4,4,5,99,0))
    println(Day2.run(machine4))
    val machine5 = Machine(0,List(1,1,1,4,99,5,6,0,99))
    println(Day2.run(machine5))
    println("="*10)
    val machine6 = Machine(0, List(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,9,19,23,1,13,23,27,1,5,27,31,2,31,6,35,1,35,5,39,1,9,39,43,1,43,5,47,1,47,5,51,2,10,51,55,1,5,55,59,1,59,5,63,2,63,9,67,1,67,5,71,2,9,71,75,1,75,5,79,1,10,79,83,1,83,10,87,1,10,87,91,1,6,91,95,2,95,6,99,2,99,9,103,1,103,6,107,1,13,107,111,1,13,111,115,2,115,9,119,1,119,6,123,2,9,123,127,1,127,5,131,1,131,5,135,1,135,5,139,2,10,139,143,2,143,10,147,1,147,5,151,1,151,2,155,1,155,13,0,99,2,14,0,0))
    println(Day2.run(machine6))

    def test(noun: Int, verb: Int) = List(1,noun,verb,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,9,19,23,1,13,23,27,1,5,27,31,2,31,6,35,1,35,5,39,1,9,39,43,1,43,5,47,1,47,5,51,2,10,51,55,1,5,55,59,1,59,5,63,2,63,9,67,1,67,5,71,2,9,71,75,1,75,5,79,1,10,79,83,1,83,10,87,1,10,87,91,1,6,91,95,2,95,6,99,2,99,9,103,1,103,6,107,1,13,107,111,1,13,111,115,2,115,9,119,1,119,6,123,2,9,123,127,1,127,5,131,1,131,5,135,1,135,5,139,2,10,139,143,2,143,10,147,1,147,5,151,1,151,2,155,1,155,13,0,99,2,14,0,0)

    val outputs: LazyList[Either[Machine,Machine]] = for {
      noun <- (0 until 1000).to(LazyList)
      verb <- (0 until 1000).to(LazyList)
    } yield Day2.run(Machine(0,test(noun,verb)))
    println("="*10)
    val answer = outputs.find({
      case Right(machine) if machine.mem.head == 19690720 => true
      case _ => false
    })
    println(answer)
  }
}
