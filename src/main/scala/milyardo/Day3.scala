package milyardo

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

/**
  * --- Day 3: Crossed Wires ---
  * The gravity assist was successful, and you're well on your way to the Venus refuelling station. During the rush back on Earth, the fuel management system wasn't completely installed, so that's next on the priority list.
  *
  * Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line of text (your puzzle input).
  *
  * The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for this measurement. While the wires do technically cross right at the central port where they both start, this point does not count, nor does a wire count as crossing with itself.
  *
  * For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8, up 5, left 5, and finally down 3:
  *
  * ...........
  * ...........
  * ...........
  * ....+----+.
  * ....|....|.
  * ....|....|.
  * ....|....|.
  * .........|.
  * .o-------+.
  * ...........
  * Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:
  *
  * ...........
  * .+-----+...
  * .|.....|...
  * .|..+--X-+.
  * .|..|..|.|.
  * .|.-X--+.|.
  * .|..|....|.
  * .|.......|.
  * .o-------+.
  * ...........
  * These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.
  *
  * Here are a few more examples:
  *
  * R75,D30,R83,U83,L12,D49,R71,U7,L72
  * U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
  * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  * U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
  * What is the Manhattan distance from the central port to the closest intersection?
  *
  * --- Part Two ---
  * It turns out that this circuit is very timing-sensitive; you actually need to minimize the signal delay.
  *
  * To do this, calculate the number of steps each wire takes to reach each intersection; choose the intersection where the sum of both wires' steps is lowest. If a wire visits a position on the grid multiple times, use the steps value from the first time it visits that position when calculating the total value of a specific intersection.
  *
  * The number of steps a wire takes is the total number of grid squares the wire has entered to get to that location, including the intersection being considered. Again consider the example from above:
  *
  * ...........
  * .+-----+...
  * .|.....|...
  * .|..+--X-+.
  * .|..|..|.|.
  * .|.-X--+.|.
  * .|..|....|.
  * .|.......|.
  * .o-------+.
  * ...........
  * In the above example, the intersection closest to the central port is reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the second wire for a total of 20+20 = 40 steps.
  *
  * However, the top-right intersection is better: the first wire takes only 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.
  *
  * Here are the best steps for the extra examples from above:
  *
  * R75,D30,R83,U83,L12,D49,R71,U7,L72
  * U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
  * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  * U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
  * What is the fewest combined steps the wires must take to reach an intersection?
  */
object Day3 extends IOApp {
  sealed trait Direction
  object Direction {
    case object Up    extends Direction
    case object Down  extends Direction
    case object Left  extends Direction
    case object Right extends Direction
  }
  case class Segment(dir: Direction, distance: Int)
  case class Wire(segments: List[Segment])

  def maxHeight(max: Int, current: Int, segment: Segment): (Int, Int) = {
    segment match {
      case Segment(Direction.Up, mag) =>
        val newHeight = current + mag
        (Math.max(max, newHeight), newHeight)
      case Segment(Direction.Down, mag) =>
        val newHeight = current - mag
        (Math.max(max, newHeight), newHeight)
      case _ => (max, current)
    }
  }

  def maxWidth(max: Int, current: Int, segment: Segment): (Int, Int) = {
    segment match {
      case Segment(Direction.Right, mag) =>
        val newWidth = current + mag
        (Math.max(max, newWidth), newWidth)
      case Segment(Direction.Left, mag) =>
        val newWidth = current - mag
        (Math.max(max, newWidth), newWidth)
      case _ => (max, current)
    }
  }

  def bounds(ws: List[Wire]): (Int, Int) = {
    ws.map(bounds)
      .foldLeft((0, 0))((state, bound) => {
        val (maxH, maxW) = state
        val (h, w)       = bound
        (Math.max(maxH, h), Math.max(maxW, w))
      })
  }

  def bounds(wire: Wire): (Int, Int) = {
    val (maxH, _, maxW, _) =
      wire.segments.foldLeft((0, 0, 0, 0))((state, segment) => {
        val (maxH, curH, maxW, curW) = state
        val (newMaxW, newCurW)       = maxWidth(maxW, curW, segment)
        val (newMaxH, newCurH)       = maxHeight(maxH, curH, segment)
        (newMaxH, newCurH, newMaxW, newCurW)
      })
    (maxH, maxW)
  }

  def parse(s: String): List[Wire] =
    for {
      line <- s.split("\n").toList
      segments = line
        .split(",")
        .map({ s =>
          val dir = s.charAt(0).toUpper match {
            case 'U' => Direction.Up
            case 'D' => Direction.Down
            case 'R' => Direction.Right
            case 'L' => Direction.Left
          }
          val magnitude = s.substring(1).toInt
          Segment(dir, magnitude)
        })
        .toList
    } yield Wire(segments)

  def draw(ws: List[Wire]): String = {
    val (height, width) = bounds(ws)
    val grid            = Vector.fill(height, width)(".")

    grid.map(_.mkString("")).mkString("\n")
  }

  def coords(wire: Wire): List[(Int, Int)] =
    wire.segments.foldLeft(List((0, 0)))((points, segment) => {
      val (x, y) = points.head
      val moves = segment match {
        case Segment(Direction.Up, distance) =>
          for {
            n <- 1 to distance
          } yield (x, y + n)
        case Segment(Direction.Down, distance) =>
          for {
            n <- 1 to distance
          } yield (x, y - n)
        case Segment(Direction.Left, distance) =>
          for {
            n <- 1 to distance
          } yield (x - n, y)
        case Segment(Direction.Right, distance) =>
          for {
            n <- 1 to distance
          } yield (x + n, y)
      }
      moves.reverse.toList ::: points
    })

  def intersections(cs: List[List[(Int, Int)]]): Set[(Int, Int)] =
    cs.map(_.toSet).reduce(_ intersect _).filter(_ != (0, 0))

  def distanceManhattanFromOrigin(xy: (Int, Int)): Int =
    Math.abs(xy._1) + Math.abs(xy._2)

  def lengthToPoint(xy: (Int, Int), cs: List[(Int, Int)]): Int = cs.indexOf(xy)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      input <- loadResource("input_3.txt").use(IO.pure)
      wires = parse(input)
      cs    = wires.map(coords)
      _ <- console(s"wire lengths: ${cs.map(_.size)}")
      is = intersections(cs)
      _ <- console(s"intersections: ${is.size}")
      closest = is.map(distanceManhattanFromOrigin).min
      shortest = (for {
        wire  <- cs.map(_.reverse)
        point <- is
      } yield point -> lengthToPoint(point, wire))
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).sum)
        .minBy(_._2)
      _ <- console(closest)
      _ <- console(shortest)
    } yield ExitCode.Success
}
