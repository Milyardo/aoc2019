package milyardo

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.collection.mutable

/**
  * --- Day 4: Secure Container ---
  * You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.
  *
  * However, they do remember a few key facts about the password:
  *
  * It is a six-digit number.
  * The value is within the range given in your puzzle input.
  * Two adjacent digits are the same (like 22 in 122345).
  * Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
  * Other than the range rule, the following are true:
  *
  * 111111 meets these criteria (double 11, never decreases).
  * 223450 does not meet these criteria (decreasing pair of digits 50).
  * 123789 does not meet these criteria (no double).
  * How many different passwords within the range given in your puzzle input meet these criteria?
  *
  * Your puzzle input is 271973-785961.
  *
  * --- Part Two ---
  * An Elf just remembered one more important detail: the two adjacent matching digits are not part of a larger group of matching digits.
  *
  * Given this additional criterion, but still ignoring the range rule, the following are now true:
  *
  * 112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
  * 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
  * 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
  * How many different passwords within the range given in your puzzle input meet all of the criteria?
  */
object Day4 extends IOApp {
  def isSixDigit(n: String): Boolean = n.length == 6
  //wtf bad
  def hasOnly2Repeat(n: String) = {
    var hasTwo: Boolean = false
    var count           = 1
    var prev: Char      = '_'
    for {
      c <- n + '_'
    } {
      if (c == prev) {
        count = count + 1
      } else {
        if (count == 2) {
          hasTwo = true
        }
        count = 1
      }
      prev = c
    }
    hasTwo
  }

  def decreases(n: String) =
    n.zip(n.tail).map(c => c._1 > c._2).reduce(_ || _)

  def find(range: Range) =
    for {
      n <- range
      str = n.toString
      if hasOnly2Repeat(str) && isSixDigit(str) && !decreases(str)
    } yield n

  def hasRepeat(n: String) =
    n.zip(n.tail).map(c => c._1 == c._2).reduce(_ || _)

  override def run(args: List[String]): IO[ExitCode] =
    for {
//      _ <- console(hasRepeat("12345"))
//      _ <- console(hasRepeat("112345"))
//      _ <- console(hasRepeat("123345"))
//      _ <- console("*" * 5)
//      _ <- console(decreases("123345"))
//      _ <- console(decreases("54321"))
//      _ <- console(decreases("111111"))
//      _ <- console(decreases("223450"))
//      _ <- console("*" * 5)
      _ <- console(hasOnly2Repeat("112233"))
      _ <- console(hasOnly2Repeat("123444"))
      _ <- console(hasOnly2Repeat("111122"))
      _ <- console(hasOnly2Repeat("123422"))
      _ <- console("*" * 5)
      _ <- console(find(271973 to 785961).size)
    } yield ExitCode.Success
}
