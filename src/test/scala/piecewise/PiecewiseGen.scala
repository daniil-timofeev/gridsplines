package piecewise

import org.scalacheck.Arbitrary.arbDouble
import org.scalacheck.Gen.{nonEmptyListOf, _}
import org.scalacheck.{Arbitrary, Gen}
import piecewise.SplineCheck.listOfDoublesGen

object PiecewiseGen {


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

  val points: Gen[List[(Double, Double)]] =
    listOfDoublesGen.map(_.distinct.sorted)
      .flatMap((x: List[Double]) => {
        var max = 0.0
        x.map(x => {
          choose(max, 100.0)
            .map(y => {
              max = y
              List((x, y))
            })
        }).reduce((a, b) => {
          for {
            aa <- a
            bb <- b
          } yield aa ++ bb
        })
      }) suchThat(list => list.lengthCompare(3) == 1)

  implicit val ArbPoints = Arbitrary(points)

}
