package piecewise.intervaltree

import com.twitter.algebird.{InclusiveLower, Intersection}
import com.twitter.algebird.Interval.InLowExUp


import scala.annotation.tailrec


/**
  * Created by Даниил on 24.03.2017.
  */
abstract class IntervalTree[K: Ordering, V](val interval: InLowExUp[K], val v: V){

  def sliceUpper(x: K): IntervalTree[K, V]

  def sliceLower(x: K): IntervalTree[K, V]

}

object IntervalTree{

  @tailrec
  def interval[K, V](x: K, tree: Option[IntervalTree[K, V]]): Option[IntervalTree[K, V]] = {
    if(tree.nonEmpty) tree.get match{
      case internal: InternalNode[K, V] =>{
        x match{
          case center if internal.interval.contains(center) => tree
          case left if internal.left.nonEmpty && internal.interval.upper.contains(left) => interval(x, internal.left)
          case right if internal.right.nonEmpty && internal.interval.lower.contains(right) => interval(x, internal.right)
          case _  => None
        }
      }
      case leaf: Leaf[K, V] =>{
        if(leaf.interval.contains(x)) tree else None
    }
  } else None
}

  def apply[K: Ordering, V](list: Vector[(InLowExUp[K], V)]): Option[IntervalTree[K, V]] = {
    val sorted = list.sortBy(t => t._1)
    buildLeft(sorted).result
  }



  case class InternalNode[K, V](override val interval: InLowExUp[K],
                                     override val v: V, left: Option[IntervalTree[K, V]], right: Option[IntervalTree[K, V]]) extends IntervalTree[K, V](interval, v){
    def this(map: (InLowExUp[K], V), left: Option[IntervalTree[K, V]], right: Option[IntervalTree[K, V]]){
      this(map._1, map._2, left, right)
    }

    override
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

    override
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
  case class Leaf[K, V](override val interval: InLowExUp[K], override val v: V) extends IntervalTree[K, V](interval, v){
    def this(map: (InLowExUp[K], V)){
      this(map._1, map._2)
    }

    override
    def sliceUpper(x: K): IntervalTree[K, V] = {
      if(interval.contains(x)) this.copy(interval = Intersection.apply(interval.lower, interval.upper.copy(x)))
      else this //assumed what intervals  at tree are continuous
    }

    override
    def sliceLower(x: K): IntervalTree[K, V] = {
      if(interval.contains(x)) this.copy(interval = Intersection.apply(interval.lower.copy(x), interval.upper))
      else this //assumed what intervals at tree are continuous
    }

  }

  import scala.util.control.TailCalls._
  protected def buildLeft[K, V](vals: Vector[(InLowExUp[K], V)]): TailRec[Option[IntervalTree[K, V]]] = {
    vals match{
      case Vector(one) => done(Some(new Leaf[K, V](one)))
      case Vector(leaf, internal) => done(Some(new InternalNode[K, V](internal, Some(new Leaf[K, V](leaf)), None)))
      case Vector(leftLeaf, internal, rightLeaf) => {
        done(Some(new InternalNode[K, V](internal, Some(new Leaf[K, V](leftLeaf)), Some(new Leaf[K, V](rightLeaf)))))
      }
      case default: Vector[((InLowExUp[K], V))] => {
        val size = vals.size - 1
        val leftSize =
          if(size % 2 == 0) (size + 1) / 2
          else size / 2
        val rightSize = size - leftSize - 1
        for{
          leftNode: Option[IntervalTree[K, V]] <- tailcall(buildLeft(default.take(leftSize)))
          rightNode: Option[IntervalTree[K, V]] <- tailcall(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftSize), leftNode, rightNode))
      }
    }}

  protected def buildRight[K, V](vals: Vector[(InLowExUp[K], V)]): TailRec[Option[IntervalTree[K, V]]] = {
    vals match{
      case Vector(one) => done(Some(new Leaf[K, V](one)))
      case Vector(internal, leaf) => done(Some(new InternalNode[K, V](internal, None, Some(new Leaf[K, V](leaf)))))
      case Vector(leftLeaf, internal, rightLeaf) => {
        done(Some(new InternalNode[K, V](internal, Some(new Leaf[K, V](leftLeaf)), Some(new Leaf[K, V](rightLeaf)))))
      }
      case default: Vector[((InLowExUp[K], V))] => {
        val size = vals.size - 1
        val leftSize =
          if(size % 2 == 0) (size - 1) / 2
          else size / 2
        val rightSize = size - leftSize - 1
        for{
          leftNode: Option[IntervalTree[K, V]] <- tailcall(buildLeft(default.take(leftSize)))
          rightNode: Option[IntervalTree[K, V]] <- tailcall(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftSize), leftNode, rightNode))
      }
    }
  }

}