package piecewise
import com.twitter.algebird._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._
import piecewise.intervaltree._
object IntervalTreeIteratorsCheck extends Properties("Interval Tree Iterators"){

  val list = nonEmptyListOf(Arbitrary.arbDouble.arbitrary).suchThat(_.size > 3)

  val intervals = list.map{list =>
    list.sorted.sliding(2).collect{
      case List(l0, l1) if !Interval.leftClosedRightOpen[Double](l0, l1).isEmpty => {
        (l0, l1)
      }
    }.toList
  }

  property(" Size equality") = forAll(intervals){list =>
    val size = list.size
    val values = Iterator.from(1, 1)
    val tree = IntervalTree.buildRight(list.iterator.zip(values), size)
    tree.size ?= size
  }

  property(" Array based iterator") = forAll(intervals){list =>
    val size = list.size
    val values = Iterator.from(1, 1)
    val tree = IntervalTree.buildRight(list.iterator.zip(values), size)
    tree.iterator.size ?= size
  }



}
