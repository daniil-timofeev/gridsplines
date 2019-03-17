package piecewise
package intervaltree

import cats.instances.double._
import org.scalacheck.Gen.infiniteStream
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import piecewise.PiecewiseGen._

/**
  * Created by daniil-timofeev on 21.04.2017.
  */
object IntervalTreeCheck extends Properties("Interval Tree"){

  val values = infiniteStream[Int](Arbitrary.arbInt.arbitrary)
  val sources = for{
    intr <- points
    vals <- values
  } yield {
    (intr zip vals)
  }

  property(" Can build") =
    forAll(sources){s: List[((Double, Double), Int)] =>
        val tree = AbsITree.build(s.iterator, s.size)
        tree.nonEmpty
  }

}

