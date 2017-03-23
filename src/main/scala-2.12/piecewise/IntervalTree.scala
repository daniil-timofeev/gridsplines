package piecewise
import com.twitter.algebird.Interval.{InLowExUp, MaybeEmpty}
import com.twitter.algebird._
import piecewise.IntervalTree.InternalNode

import scala.annotation.tailrec
import scala.util.control.TailCalls.{TailRec, done, tailcall}
/**
  *
  */
class IntervalTree[K: Ordering, V](val root: InternalNode[K, V]) {

}
object IntervalTree{

  class IntervalOrdering[L <: Lower[K], U <: Upper[K], K] extends Ordering[Intersection[L, U, K]]{
    override def compare(x: Intersection[L, U, K], y: Intersection[L, U, K]): Int = {
      x.intersect(y) match{
        case empty: Empty[_] =>{
          if(x.lower.intersects(y.upper.asInstanceOf[Upper[K]])) 1 else -1
        }
        case _ => 0
      }
    }
  }
  implicit object InLowExUpOrdering extends IntervalOrdering[InclusiveLower[Double], ExclusiveUpper[Double], Double]
  implicit object ExLowInUpOrdering extends IntervalOrdering[ExclusiveLower[Double], InclusiveUpper[Double], Double]


  def apply[K: Ordering, V](list: Vector[(InLowExUp[K], V)]): Unit = {
    val sorted = list.sortBy(t => t._1)
    buildLeft(sorted)
  }

  protected abstract class Node[K, V](val interval: InLowExUp[K],val v: V)

  protected class InternalNode[K, V](override val interval: InLowExUp[K],
  override val v: V, val left: Option[Node[K, V]], val right: Option[Node[K, V]]) extends Node[K, V](interval, v){
    def this(map: (InLowExUp[K], V), left: Option[Node[K, V]], right: Option[Node[K, V]]){
      this(map._1, map._2, left, right)
    }
  }
  protected class Leaf[K, V](override val interval: InLowExUp[K], override val v: V) extends Node[K, V](interval, v){
    def this(map: (InLowExUp[K], V)){
      this(map._1, map._2)
    }

  }
  import scala.util.control.TailCalls._
  protected def buildLeft[K, V](vals: Vector[(InLowExUp[K], V)]): TailRec[Option[Node[K, V]]] = {
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
          leftNode: Option[Node[K, V]] <- tailcall(buildLeft(default.take(leftSize)))
          rightNode: Option[Node[K, V]] <- tailcall(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftSize), leftNode, rightNode))
      }
    }}

  protected def buildRight[K, V](vals: Vector[(InLowExUp[K], V)]): TailRec[Option[Node[K, V]]] = {
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
          leftNode: Option[Node[K, V]] <- tailcall(buildLeft(default.take(leftSize)))
          rightNode: Option[Node[K, V]] <- tailcall(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftSize), leftNode, rightNode))
      }
    }
  }

}