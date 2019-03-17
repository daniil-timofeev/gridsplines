package piecewise.intervaltree

import org.scalacheck.Gen
import org.scalacheck.Gen.chooseNum

object Gens {

  def intervalsGen(minBound: Double, n: Int): Gen[List[(Double, Double)]] =
    if (n == 0) Gen.const(Nil)
    else for {
      maxBound <- chooseNum(minBound + 1.0, minBound + 10.0)
      tail <- intervalsGen(maxBound, n - 1)
    } yield {(minBound, maxBound) :: tail}

  def headBound(ls: List[((Double, Double), Int)]): Double = ls.head._1._1
  def lastBound(ls: List[((Double, Double), Int)]): Double = ls.last._1._2

}
