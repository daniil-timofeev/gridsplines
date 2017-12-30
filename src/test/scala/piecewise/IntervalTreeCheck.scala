package piecewise

import org.scalacheck.Gen.{infiniteStream, _}
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import piecewise.intervaltree._
/**
  * Created by daniil-timofeev on 21.04.2017.
  */
object IntervalTreeCheck extends Properties("Interval Tree"){

  val doubleInterSection: org.scalacheck.Gen[(Double, Double)] =
    for{
      low <- Arbitrary.arbDouble.arbitrary
      upp <- Arbitrary.arbDouble.arbitrary.suchThat(u => u > low)
    } yield{(low, upp)}


  val intervals = listOfN(5, doubleInterSection)
  val values = infiniteStream[Int](Arbitrary.arbInt.arbitrary)
  val sources = for{
    intr <- intervals
    vals <- values
  } yield {
    (intr zip vals)
  }

  property(" Can build") =
    forAll(sources suchThat(list => list.size > 0)){
      (s: List[((Double, Double), Int)]) =>
        val tree = IntervalTree.buildLeft(s.iterator, s.size)
        tree.nonEmpty
    }
}

