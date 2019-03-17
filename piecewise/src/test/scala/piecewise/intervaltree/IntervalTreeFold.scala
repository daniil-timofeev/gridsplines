package piecewise.intervaltree
import cats.instances.double._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}
import piecewise.intervaltree.Gens._

object IntervalTreeFold extends Properties("Interval Tree Fold"){

  def segments(s: Int, n: Int): Gen[List[Int]] =
    if (n == 0) Gen.const(s :: Nil)
    else for {
     head <- if (s == 0) Gen.const(0) else choose(0, s)
     tail <- segments(s - head, n - 1)
    } yield {head :: tail}


  def defaultNodes(min: Double,
                   n: Int): Gen[List[((Double, Double), Int)]] =
    for {
      ins <- intervalsGen(min, n)
      vls <- listOfN(ins.size, chooseNum(0, 100))
    } yield {ins zip vls}


  def knownSumNodes(min: Double,
                    n: Int,
                    sum: Int): Gen[(Int, List[((Double, Double), Int)])] =
    for {
      ins <- intervalsGen(min, n)
      vls <- segments(sum, ins.size)
    } yield {
        val zip = ins zip vls
        val sum = zip.map(_._2).sum
       (sum, zip)
    }

  val genMid: Gen[(Int, Double, Double, List[((Double, Double), Int)])] =
    for {
      knownSum <- chooseNum(10, 100)
      left <- defaultNodes(-100.0, 7)
      knownNodes <- knownSumNodes(lastBound(left), 10, knownSum)
      right <- defaultNodes(lastBound(knownNodes._2), 9)
    } yield {(knownNodes._1,
            headBound(knownNodes._2),
            lastBound(knownNodes._2),
            left ::: knownNodes._2 ::: right)
    }

  val genLeft: Gen[(Int, Double, Double, List[((Double, Double), Int)])] =
    for {
      knownSum <- chooseNum(10, 100)
      knownNodes <- knownSumNodes(-100.0, 10, knownSum)
      right <- defaultNodes(lastBound(knownNodes._2), 9)
    } yield {(knownNodes._1,
              headBound(knownNodes._2),
              lastBound(knownNodes._2),
              knownNodes._2 ::: right)}


  val genRight: Gen[(Int, Double, Double, List[((Double, Double), Int)])] =
    for {
      knownSum <- chooseNum(10, 100)
      left <- defaultNodes(-100.0, 7)
      knownNodes <- knownSumNodes(lastBound(left), 10, knownSum)
    } yield {(knownNodes._1,
              headBound(knownNodes._2),
              lastBound(knownNodes._2),
              left ::: knownNodes._2)}

  val genFragment: Gen[(Int, Double, Double, List[((Double, Double), Int)])] =
    for {
      knownSum <- chooseNum(10, 100)
      left <- defaultNodes(-100.0, 7)
      knownNodes <- knownSumNodes(lastBound(left), 10, knownSum)
      right <- defaultNodes(lastBound(knownNodes._2), 15)
    } yield {(knownNodes._1,
              headBound(knownNodes._2),
              lastBound(knownNodes._2),
              left ::: knownNodes._2 ::: right)}

  val checkSum = {t: (Int, Double, Double, List[((Double, Double), Int)]) =>
    val (sum, low, upp, src) = t
    val tree = AbsITree.build(src.iterator, src.size)
    val newSum =
      AbsITree.viewFold(low, upp)(tree)((l, u, v) => if (l - u >= 0.0) 0.0 else v)
    newSum ?= sum
  }

  property(" Fold mid") = forAll(genMid){checkSum}

  property(" Fold left") = forAll(genLeft){checkSum}

  property(" Fold right") = forAll(genRight){checkSum}

  property(" Fold fragment") = forAll(genFragment){checkSum}

}
