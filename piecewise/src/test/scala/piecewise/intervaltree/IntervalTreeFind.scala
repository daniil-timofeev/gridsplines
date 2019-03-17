package piecewise.intervaltree
import cats.instances.double._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._
import piecewise.intervaltree.Gens._

object IntervalTreeFind extends Properties("Interval tree find"){

  val answer = 42

  def defaultNodes(min: Double, n: Int): Gen[List[((Double, Double), Int)]] =
    for {
      ins <- intervalsGen(min, n)
      vls <- listOfN(n, chooseNum(0, 41))
    } yield {ins zip vls}

  val gen: Gen[(Double, List[((Double, Double), Int)])] = for {
    leftSize <- chooseNum(1, 7)
    left <- defaultNodes(0.0, leftSize)
    midI <- intervalsGen(lastBound(left), 1) suchThat (_.size == 1)
    where <- chooseNum(midI.head._1, midI.head._2)
      .suchThat(d =>
        Interval.contains(CsdOpn.pure.unsafe(midI.head._1, midI.head._2))(d)
      )
    rightSize <- chooseNum(1, 7)
    right <- defaultNodes(midI.head._2, rightSize)
  } yield (where, left ::: List((midI.head, answer)) ::: right)

  property(" find answer") =
    forAll(gen){src =>
      val (where, values) = src
      val tree = AbsITree.build(values)
      val res = AbsITree.find(where, tree)(0)(_.value)
      res ?= answer
  }

}
