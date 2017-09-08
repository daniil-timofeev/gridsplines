package piecewise

import org.scalacheck.Arbitrary.arbDouble
import org.scalacheck.Gen
import org.scalacheck.Gen.nonEmptyListOf
import org.scalacheck.Arbitrary
import org.scalacheck.Gen._

object GenPiecewice {


  val xCoords: Gen[Set[Double]] =
    nonEmptyListOf(choose(
      Double.MinValue * 0.005,
      Double.MaxValue * 0.005)).
      map(_.toSet).
      suchThat(set => set.size > 3)

  val anyPoints: Gen[List[(Double, Double)]] = for{
    xVals: Set[Double] <- xCoords
  } yield xVals.
    toList.
    zip(xVals.toList.map(x => arbDouble.arbitrary.sample.get)).
    sortBy(_._1)


  val anyPositivePoints: Gen[List[(Double, Double)]] = for{
    xVals: Set[Double] <- xCoords
  } yield xVals.
    toList.
    zip(xVals.toList.map(_ => posNum[Double].sample.getOrElse(0.0))).
    sortBy(_._1)

}
