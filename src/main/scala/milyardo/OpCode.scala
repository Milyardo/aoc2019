package milyardo

import cats._
import cats.implicits._
import scala.annotation.tailrec

sealed trait OpCode

object OpCode {
  final case object Exit                                       extends OpCode
  final case object Halt                                       extends OpCode
  final case class Add(left: Param, right: Param, dest: Param) extends OpCode
  final case class Mul(left: Param, right: Param, dest: Param) extends OpCode
  final case class Input(dest: Param)                          extends OpCode
  final case class Output(src: Param)                          extends OpCode

  def nextOp(program: List[Int]): OpCode = {
    def lastTwo(n: Int)            = n                                 % 100
    def digitAtN(n: Int, pos: Int) = (n / Math.pow(10, pos - 1).toInt) % 10

    program match {
      case n :: _ if lastTwo(n) == 99 _ => Exit
      case n :: left :: right :: dest :: _ if lastTwo(n) == 1 =>
        Add(
          Param(left, digitAtN(n, 3)),
          Param(right, digitAtN(n, 4)),
          Param(dest, 0)
        )
      case n :: left :: right :: dest :: _ if lastTwo(n) == 2 =>
        Mul(
          Param(left, digitAtN(n, 3)),
          Param(right, digitAtN(n, 4)),
          Param(dest, 0)
        )
      case n :: dest :: _ if lastTwo(n) == 3 =>
        Input(Param(dest, 0))
      case n :: src :: _ if lastTwo(n) == 4 =>
        Output(Param(src, 0))
      case _ :: _ =>
        Halt
      case Nil =>
        Halt
    }
  }
}

final case class Param(value: Int, mode: Int) {
  def deref(mem: List[Int]): Option[Int] = mode match {
    case 0 if value < mem.length && value >= 0 =>
      Some(mem(value))
    case 0 => None
    case 1 =>
      Some(value)
  }
}

final case class Machine(pc: Int, mem: List[Int])
object Machine {
  def step(machine: Machine): Either[Machine, Machine] = {
    import OpCode._
    def mutate(
        left: Param,
        right: Param,
        dest: Param,
        op: (Int, Int) => Int
    ): Either[Machine, Machine] =
      (
        left.deref(machine.mem),
        right.deref(machine.mem),
        dest.deref(machine.mem)
      ).mapN((l, r, d) => machine.mem.updated(d, op(l, r)))
        .toRight(machine)
        .map(newMem => machine.copy(pc = machine.pc + 4, mem = newMem))

    val nextOp = OpCode.nextOp(machine.mem.drop(machine.pc))
    nextOp match {
      case Exit => Right(machine.copy(pc = Int.MaxValue))
      case Halt => Left(machine)
      case Add(left, right, dest) =>
        mutate(left, right, dest, _ + _)
      case Mul(left, right, dest) =>
        mutate(left, right, dest, _ * _)
      case Input(dest) =>
        dest
          .deref(machine.mem)
          .toRight(machine)
          .map({ d =>
            print("\n> ")
            val in = scala.io.StdIn.readLine().toInt
            machine.copy(mem = machine.mem.updated(d, in), pc = machine.pc + 2)
          })
      case Output(src) =>
        src
          .deref(machine.mem)
          .toRight(machine)
          .map({ out =>
            println(out)
            machine.copy(pc = machine.pc + 2)
          })
    }
  }

  def run(init: Machine): Either[Machine, Machine] = {
    @tailrec def loop(
        state: Either[Machine, Machine]
    ): Either[Machine, Machine] =
      state match {
        case fail @ Left(_)                                     => fail
        case done @ Right(Machine(pc, mem)) if pc >= mem.length => done
        case Right(continue)                                    => loop(step(continue))
      }
    loop(Right(init))
  }

  def load(program: String): Machine = {
    val mem = program
      .split(",")
      .foldLeft(List.empty[Int])((acc, code) => code.toInt :: acc)
      .reverse
    Machine(0, mem)
  }
}
