package piecewise
import com.twitter.algebird.Interval.MaybeEmpty.{NotSoEmpty, SoEmpty}
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import com.twitter.algebird._
import com.twitter.algebird.Interval._
import piecewise.intervaltree.IntervalTree
object IntervalTreeIterators extends Properties("Interval Tree Iterators"){

  val list = nonEmptyListOf(Arbitrary.arbDouble.arbitrary).suchThat(_.size > 3)

  val intervals = list.map{list =>
    val sorted = list.sorted
    sorted.sliding(2).collect{
      case List(l0, l1) if !Interval.leftClosedRightOpen[Double](l0, l1).isEmpty => {
        Interval.leftClosedRightOpen[Double](l0, l1).asInstanceOf[NotSoEmpty[Double, InLowExUp]].get
      }
    }.toList
  }

  property(" Size equality") = forAll(intervals){list =>
    val size = list.size
    val values = Iterator.from(1, 1)
    val Some(tree) =
    IntervalTree.buildLeft(list.iterator.zip(values), size)
    tree.mapIterator((i, v) => (Iterator.single((i, v)), 1))._2 ?= tree.size
  }

  property(" Array based iterator") = forAll(intervals){list =>
    val size = list.size
    val values = Iterator.from(1, 1)
    val Some(tree) =
      IntervalTree.buildLeft(list.iterator.zip(values), size)
    tree.iterator.size ?= tree.size
  }



}
