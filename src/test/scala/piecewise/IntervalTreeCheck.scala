package piecewise

import org.scalacheck.Gen.{infiniteStream, _}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}
import piecewise.SplineCheck.listOfDoublesGen
import piecewise.intervaltree._
/**
  * Created by daniil-timofeev on 21.04.2017.
  */
object IntervalTreeCheck extends Properties("Interval Tree"){

  val intervals: Gen[List[(Double, Double)]] =
    listOfDoublesGen.map(_.distinct.sorted)
      .flatMap((x: List[Double]) => {
        x.map(x => choose(x, 100.0).map(y => List((x, y))))
          .reduce((a, b) => {
            a.flatMap((a: List[(Double, Double)]) =>
              b.map((b: List[(Double, Double)]) => a ++ b))
        })
      }) suchThat(list => list.lengthCompare(0) == 1)

  val values = infiniteStream[Int](Arbitrary.arbInt.arbitrary)
  val sources = for{
    intr <- intervals
    vals <- values
  } yield {
    (intr zip vals)
  }

  property(" Can build") =
    forAll(sources){
      (s: List[((Double, Double), Int)]) =>
        val tree = IntervalTree.buildLeft(s.iterator, s.size)
        tree.nonEmpty
  }

}

