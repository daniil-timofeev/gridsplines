package piecewise.intervaltree

import com.twitter.algebird.{InclusiveLower, Intersection, Monoid}
import com.twitter.algebird.Interval.InLowExUp

import scala.Option
import com.twitter.algebird.monad._
import com.twitter.algebird.monad.Trampoline._
import piecewise.PieceFunction

import scala.annotation.tailrec


/**
  * Created by Даниил on 24.03.2017.
  */
abstract class IntervalTree[K: Ordering, V](val interval: InLowExUp[K], val v: V){

  def apply = v

  def sliceUpper(x: K): IntervalTree[K, V]

  def sliceLower(x: K): IntervalTree[K, V]

}

object IntervalTree{

  @tailrec
  def find[K: Ordering, V](x: K, tree: Option[IntervalTree[K, V]]): Option[IntervalTree[K, V]] = {
    if(tree.nonEmpty) tree.get match{
      case internal: InternalNode[K, V] =>{
        x match{
          case center if internal.interval.contains(center) => tree
          case left if internal.left.nonEmpty && internal.interval.upper.contains(left) => find(x, internal.left)
          case right if internal.right.nonEmpty && internal.interval.lower.contains(right) => find(x, internal.right)
          case _  => None
        }
      }
      case leaf: Leaf[K, V] => {
        if(leaf.interval.contains(x)) tree else None
    }
  } else None
}

  def apply[K: Ordering, V](vect: List[(InLowExUp[K], V)]): Option[IntervalTree[K, V]] = {

    implicit object Ordering extends Ordering[InLowExUp[K]]{
      override def compare(x: InLowExUp[K], y: InLowExUp[K]): Int =
        if(x.lower.contains(y.lower.lower)){
          if(x.upper.contains(y.upper.upper)) 0
          else - 1
        }
        else if(y.lower.contains(x.lower.lower) && y.upper.contains(x.upper.upper)) 0
        else 1
    }

    val sorted = vect.sortBy(t => t._1)
    Trampoline.run(buildLeft(sorted))
  }


  def area[K: Ordering, V](lower: K, upper: K, tree: Option[IntervalTree[K, V]]): Trampoline[List[(InLowExUp[K], V)]] = {
    def contain(interval: InLowExUp[K]): Boolean = {
      interval.lower.contains(upper) || interval.upper.contains(lower)
    }
    //TODO improve speed
    tree match{
      case Some(InternalNode(interval, v, left, right)) => {
        val mid = if(contain(interval)) (interval, v) :: Nil else Nil
        for{
          l <- call(area[K, V](lower, upper, left))
          r <- call(area[K, V](lower, upper, right))
        }yield{ l ::: mid ::: r }
      }
      case Some(Leaf(interval, v)) if contain(interval) => Done((interval, v) :: Nil)
      case _ => Done(Nil)
    }
  }

  def toList[K: Ordering, V](tree: Option[IntervalTree[K, V]])
  : Trampoline[List[(InLowExUp[K], V)]] = {
    tree match{
      case None => Done(List.empty[(InLowExUp[K], V)])
      case Some(InternalNode(interval, v, left, right)) => {
        for {
          l <- call(toList(left))
          r <- call(toList(right))
        } yield {
          l ::: List[(InLowExUp[K], V)]((interval, v)) ::: r
        }
      }
      case Some(Leaf(interval, v)) => Done(List((interval, v)))
    }
  }

  case class InternalNode[K: Ordering, V](
  override val interval: InLowExUp[K],
  override val v: V,
  left: Option[IntervalTree[K, V]],
  right: Option[IntervalTree[K, V]]) extends IntervalTree[K, V](interval, v){
    def this(map: (InLowExUp[K], V), left: Option[IntervalTree[K, V]], right: Option[IntervalTree[K, V]]){
      this(map._1, map._2, left, right)
    }

    implicit def monoid[K: Ordering, V] = Monoid.from[Option[IntervalTree[K, V]]](None)((l, r) =>{
      val vect = Trampoline.run(toList(l)) ::: Trampoline.run(toList(r))
      IntervalTree.apply(vect)
    })


    def sliceUpper(x: K): IntervalTree[K, V] = {
      x match{
        case center if interval.contains(x) => {
          this.copy(interval = Intersection.apply(interval.lower, interval.upper.copy(center)), right = None)
        }
        case l if left.nonEmpty && interval.upper.contains(l) => {
          this.copy(left = left.map(old => old.sliceUpper(l)), right = None)
        }
        case r if right.nonEmpty && interval.lower.contains(r) => {
          this.copy(right = right.map(old => old.sliceUpper(r)))
        }
      }
    }

    def sliceLower(x: K): IntervalTree[K, V] = {
      x match{
        case center if interval.contains(x) => {
          this.copy(interval = Intersection.apply(interval.lower.copy(center), interval.upper), left = None)
        }
        case l if left.nonEmpty && interval.upper.contains(l) => {
          this.copy(left = left.map(old => old.sliceLower(l)))
        }
        case r if right.nonEmpty && interval.lower.contains(r) => {
          this.copy(right = right.map(old => old.sliceLower(r)), left = None)
        }
      }
    }



  }

  case class Leaf[K: Ordering, V](override val interval: InLowExUp[K], override val v: V) extends IntervalTree[K, V](interval, v){

    def this(map: (InLowExUp[K], V)){
      this(map._1, map._2)
    }

    def sliceUpper(x: K): IntervalTree[K, V] = {
      if(interval.contains(x)) this.copy(interval = Intersection.apply(interval.lower, interval.upper.copy(x)))
      else this //assumed what intervals  at tree are continuous
    }

    def sliceLower(x: K): IntervalTree[K, V] = {
      if(interval.contains(x)) this.copy(interval = Intersection.apply(interval.lower.copy(x), interval.upper))
      else this //assumed what intervals at tree are continuous
    }

  }

  final def buildLeft[K: Ordering, V](vals: List[(InLowExUp[K], V)]): Trampoline[Option[IntervalTree[K, V]]] = {
    vals match{
      case one :: Nil => Done(Some(new Leaf[K, V](one)))
      case leaf :: internal :: Nil => {
        val l = new Leaf(leaf)
        Done(Some(new InternalNode[K, V](internal, Some(l), None)))
      }
      case leftLeaf :: internal :: rightLeaf :: Nil => {
        val left = new Leaf(leftLeaf)
        val right = new Leaf(rightLeaf)
        Done(Some(new InternalNode(internal, Some(left), Some(right))))
      }
      case default: List[((InLowExUp[K], V))] => {
        val size = vals.size
        val leftIndex =
          if(size % 2 == 0) (size + 1) / 2
          else size / 2
        val rightSize = size - leftIndex - 1
        for{
          leftNode <- call(buildLeft(default.take(leftIndex)))
          rightNode <- call(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftIndex), leftNode, rightNode))
      }
    }}

  final def buildRight[K: Ordering, V](vals: List[(InLowExUp[K], V)]): Trampoline[Option[IntervalTree[K, V]]] = {
    vals match{
      case one :: Nil => Done(Some(new Leaf(one)))
      case internal :: leaf :: Nil => {
        val l = new Leaf(leaf)
        Done(Some(new InternalNode[K, V](internal, None, Some(l))))
      }
      case leftLeaf :: internal :: rightLeaf :: Nil=> {
        val left = new Leaf(leftLeaf)
        val right = new Leaf(rightLeaf)
        Done(Some(new InternalNode[K, V](internal, Some(left), Some(right))))
      }
      case default: List[((InLowExUp[K], V))] => {
        val size = vals.size
        val leftIndex =
          if(size % 2 == 0) (size - 1) / 2
          else size / 2
        val rightSize = size - leftIndex - 1
        for{
          leftNode <- call(buildLeft(default.take(leftIndex)))
          rightNode <- call(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftIndex), leftNode, rightNode))
      }
    }
  }

}