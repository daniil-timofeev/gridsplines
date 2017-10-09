package piecewise

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Prop.AnyOperators
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import com.twitter.algebird.Interval._
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection, Interval}
import org.scalacheck.Gen.infiniteStream
import piecewise.intervaltree._
/**
  * Created by Даниил on 21.04.2017.
  */
object IntervalTreeTest extends Properties("Interval Tree"){


  import com.twitter.algebird.Interval.GenIntersection

  val doubleInterSection: org.scalacheck.Gen[InLowExUp[Double]] = for{
           low <- Arbitrary.arbDouble.arbitrary
           upp <- Arbitrary.arbDouble.arbitrary.suchThat(u => u > low)
  } yield{
    Intersection[InclusiveLower, ExclusiveUpper, Double](
      InclusiveLower(low), ExclusiveUpper(upp))}


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
      (s : List[(Interval.InLowExUp[Double], Int)]) =>
        val tree = NonEmptyIntervalTree(s)
        tree.nonEmpty
    }
}

