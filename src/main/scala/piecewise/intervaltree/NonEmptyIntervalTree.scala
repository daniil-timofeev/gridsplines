package piecewise.intervaltree

import com.twitter.algebird._
import com.twitter.algebird.Interval.{InLowExUp, InLowInUp}
import com.twitter.algebird.Interval.MaybeEmpty.{NotSoEmpty, SoEmpty}

import scala.Option
import com.twitter.algebird.monad._
import com.twitter.algebird.monad.Trampoline._
import piecewise._
import piecewise.intervaltree.IntervalTree.{InLowVarUp, Interv}

import scala.annotation.tailrec
import scala.collection.Iterator



/**
  * Created by Даниил on 24.03.2017.
  */

abstract class IntervalTree[K: Ordering, +V]{
  val interval: Interval[K]
  val v: V

  val size: Int

  def contains(x: K) = interval.contains(x)

  def apply = v

  def hasBoundNode: Boolean

  def sliceUpper(x: K): IntervalTree[K, V]

  def sliceLower(x: K): IntervalTree[K, V]

  def upperThan(x: K): Boolean

  def lowerThan(x: K): Boolean
}

case class EmptyNode[K: Ordering, +V]() extends IntervalTree[K, V]{
  val interval: Empty[K] = Empty[K]()
  val v = ???

  lazy val size = 0

  override def hasBoundNode = ???

  override def sliceUpper(x: K) = this

  override def sliceLower(x: K) = this

  override def upperThan(x: K) = true

  override def lowerThan(x: K) = true
}

abstract class NonEmptyIntervalTree[K: Ordering, +V](
           val interval: Intersection[InclusiveLower, Upper, K],
           val v: V)
  extends IntervalTree[K, V]{

  override
  def upperThan(x: K): Boolean = !interval.lower.contains(x)

  override
  def lowerThan(x: K): Boolean = !interval.upper.contains(x)

  def tuple: (Intersection[InclusiveLower, Upper, K], V) = (interval, v)

  import scala.collection._

  def collect[T](pf: PartialFunction[(InLowVarUp[K, U], V), T])(
    implicit bi: mutable.Builder[T, Iterator[T]]): mutable.Builder[T, Iterator[T]]

  def sumBy[T: Monoid](low: K, upp: K, f: (K, K, V) => T): T

  def map[V1](v: V => V1): NonEmptyIntervalTree[K, V1, U]

  def map[R: Ordering, V1, U1 >: U](
    f : (InLowVarUp[K, U], V) => (InLowVarUp[R, U1], V1)): NonEmptyIntervalTree[R, V1, U]

  def map[V1](f: (K, K, V) => V1): NonEmptyIntervalTree[K, V1, U]

  def splitWhere(f: (K, K, V) => Int)(
    implicit field: Field[K]): Option[NonEmptyIntervalTree[K, V, U]] = {
    import com.twitter.algebird.Operators._
    val src = iterator.flatMap{tuple =>
      val (interval, pieceFunc) = tuple
      val low = interval.lower.lower
      val upp = NonEmptyIntervalTree.extUpper(interval)
      val parts = f(low, upp, pieceFunc)
      if(parts > 1) {
        val length = upp - low
        val newLength = field.div(length, field.fromInt(parts))
        Iterator.range(0, parts - 1).map{i =>
          val newLow = low + newLength * field.fromInt(i)
          val newUpp = newLow + newLength
          new Intersection(InclusiveLower(newLow), ExclusiveUpper(newUpp))
        } ++ Iterator.single({
          val newLow = low + newLength * field.fromInt(parts)
          val newUpp = newLow + newLength
          interval match{
            case ext: InLowExUp[K] => NonEmptyIntervalTree.copyInterval(ext, newLow, newUpp)
            case inc: InLowInUp[K] => NonEmptyIntervalTree.copyInterval(inc, newLow, newUpp)
          }
        })
      } else Iterator.single((interval, pieceFunc))
    }.toIterable
    val size = src.size
   NonEmptyIntervalTree.buildLeft(src.iterator, size)
  }

  def mapIterator[U: Ordering, V1](
    f: (InLowExUp[K], V) => (Iterator[(InLowExUp[U], V1)], Int)
  ): (Iterator[(InLowExUp[U], V1)], Int)

  def buildIterator[V1 >: V](array: Array[(InLowExUp[K], V1)], start: Int)
  : Array[(InLowExUp[K], V1)]

  def iterator: Iterator[(Intersection[InclusiveLower, Upper, K], V)]

  def intervalLength(implicit g: Group[K]): K

  def lowerBound: InclusiveLower[K]

  def upperBound: InclusiveUpper[K]

  def wholeInterval: InLowInUp[K] = {
    {lowerBound && upperBound} match {
      case expected: InLowInUp[K] => expected
      case _  => ???
    }
  }

  protected def low: K = interval.lower.lower
  protected def upp: K = IntervalTree.extUpper(interval)

}

abstract class AbstractInternalNode[K: Ordering, V, U[_] <: Upper[_]](
                          override val interval: Intersection[InclusiveLower, U, K],
                          override val v: V,
                          left: IntervalTree[K, V],
                          right: IntervalTree[K, V])
  extends NonEmptyIntervalTree[K, V](interval, v){

  override lazy val size: Int = 1 + left.size + right.size


  protected def map[V1 >: V, V2](f: (K, K, V1, IntervalTree[K, V1], IntervalTree[K, V1]) =>
                                    (K, K, V2, IntervalTree[K, V2], IntervalTree[K, V2]))
    : IntervalTree[K, V2] = {
    val low = interval.lower.lower
    val upp = IntervalTree.extUpper(interval)

      val (newLow, newUpp, newV, newLeft, newRight) = f(low, upp, v, left, right)

      if (implicitly[Ordering[K]].lt(newLow, newUpp)) {
        newRight match {
          case empty: EmptyNode[K, V2] if hasBoundNode => {
            val closedInterval = Interv.closed(newLow, newUpp)
            newLeft match {
              case empty: EmptyNode[K, V2] => {
                new UpperBoundLeaf[K, V2](closedInterval, newV)
              }
              case nonEmpty: NonEmptyIntervalTree[K, V2] => {
                new UpperBoundInternalNode[K, V2](
                  closedInterval, newV, nonEmpty
                )
              }
            }
          }
          case empty: EmptyNode[K, V2] => {
            val newInterval = Interv.unsafeLeftClosedRightOpen(newLow, newUpp)
            newLeft match {
              case empty: EmptyNode[K, V2] => new Leaf[K, V2](newInterval, newV)
              case nonEmpty => new InternalNode[K, V2](newInterval, newV, newLeft, empty)
            }
          }
          case nonEmpty => {
            val newInterval = Interv.unsafeLeftClosedRightOpen(newLow, newUpp)
            new InternalNode(newInterval, newV, newLeft, nonEmpty)
          }
        }
      }
      else EmptyNode[K, V2]
  }


  def sliceUpper(x: K): IntervalTree[K, V] = {
    x match {
      case c if interval.contains(c) => {
        val low = interval.lower.lower
        val upp = IntervalTree.extUpper(interval)
        val closed = Interv.closed(low, upp)
        left match {
          case emptyLeft: EmptyNode[K, V] => new UpperBoundLeaf[K, V](closed, v)
          case nonEmptyLeft: NonEmptyIntervalTree[K, V] => {
            new UpperBoundInternalNode(closed, v, nonEmptyLeft)
          }
        }
      }
      case l if upperThan(x) => {
        left.sliceUpper(x)
      }
      case r if lowerThan(x) => {
        right match {
          case emptyRight: EmptyNode[K, V] => {
            left match {
              case emptyLeft: EmptyNode[K, V] => {
                new UpperBoundLeaf[K, V](Interv.closed(low, upp), v)
              }
              case nonEmptyLeft: NonEmptyIntervalTree[K, V] => {
                new UpperBoundInternalNode[K, V](
                  Interv.closed(low, upp), v, nonEmptyLeft
                )
              }
            }
          }
          case nonEmptyRight: NonEmptyIntervalTree[K, V] => {
            new InternalNode(
              interval.asInstanceOf[InLowExUp[K]], v, left, right.sliceUpper(x)
            )
          }
        }
      }
    }
  }


  import scala.collection._
  override
  def collect[T](pf: PartialFunction[(InLowVarUp[K], V), T])(
    implicit bi: mutable.Builder[T, Iterator[T]]): mutable.Builder[T, Iterator[T]] = {
    left.foreach(_.collect(pf))
    if(pf.isDefinedAt((interval, v))) bi += pf((interval, v))
    right.foreach(_.collect(pf))
    bi
  }

  override final def mapNonDecreasing[U: Ordering](i: (K) => U): InternalNode[U, V] = {
      interval.mapNonDecreasing(i) match {
        case e: Empty[U] => ???
        case u: Universe[U] => ???
        case newInterval: InLowExUp[U] => {
          new InternalNode[U, V](
            newInterval,
            v,
            left.map(iTree => iTree.mapNonDecreasing[U](i)),
            right.map(iTree => iTree.mapNonDecreasing[U](i))
          )
        }
      }
  }

  override final def map[V1](f: (V) => V1): InternalNode[K, V1] = {
    val contains = f(v)
    new InternalNode[K, V1](
      interval,
      contains,
      left.map(iTree => iTree.map(f)),
      right.map(iTree => iTree.map(f))
    )
  }

  override final def map[U: Ordering, V1](
                   f: (InLowExUp[K], V) => (InLowExUp[U], V1)): InternalNode[U, V1] = {
    val tuple = f(interval, v)
    new InternalNode[U, V1](tuple,
      left.map(_.map(f)),
      right.map(_.map(f))
    )
  }

  override final def map[V1](f: (K, K, V) => V1): InternalNode[K, V1] = {
    val low = interval.lower.lower
    val upp = interval.upper.upper
    new InternalNode[K, V1](
      interval,
      f(low, upp, v),
      left.map(_.map(f)),
      right.map(_.map(f))
    )
  }

  override
  def mapIterator[U: Ordering, V1](
    f: (InLowExUp[K], V) => (Iterator[(InLowExUp[U], V1)], Int)
    ): (Iterator[(InLowExUp[U], V1)], Int) = {
    val res = f(interval, v)
    if (left.isEmpty) {
      if (right.isEmpty) res
      else {
        val (result, s) = res
        val r = right.get.mapIterator(f)
        (result ++ r._1, s + r._2)
      }
    }
    else if (right.isEmpty) {
      val (result, s) = res
      val l = left.get.mapIterator(f)
      (l._1 ++ result, s + l._2)
    }
    else {
      val (result, s) = res
      val l = left.get.mapIterator(f)
      val r = right.get.mapIterator(f)
      (l._1 ++ result ++ r._1, l._2 + s + r._2)
    }
  }

  def buildIterator[V1 >: V](array: Array[(InLowExUp[K], V1)], start: Int)
  : Array[(InLowExUp[K], V1)] = {
    var idx = start
    if (left.nonEmpty) {
      left.get.buildIterator(array, start)
      idx += left.get.size
    }
    array.update(idx, tuple)
    idx += 1
    if (right.nonEmpty) {
      right.get.buildIterator(array, idx)
    }
    array
  }

  def iterator: Iterator[(InLowExUp[K], V)] = {
    val array = new Array[(InLowExUp[K], V)](size)
    buildIterator(array, 0).iterator
  }

  override lazy val toString: String = {
    val l = left.map(_.toString).getOrElse(" ")
    val r = right.map(_.toString).getOrElse(" ")
    l +
    "[" +
    interval.lower.lower.toString +
    interval.upper.upper.toString +
    ") :" + v.toString + System.lineSeparator() +
    r
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case tree: InternalNode[Any, Any] => {
        val l =
          if (this.left.isEmpty && tree.left.nonEmpty ||
            this.left.nonEmpty && tree.left.isEmpty) false
          else {
            if (this.left.isEmpty && tree.left.isEmpty) true
            else this.left.get.equals(tree.left.get)
          }

        val r =
          if (this.right.isEmpty && tree.right.nonEmpty ||
            this.right.nonEmpty && tree.right.isEmpty) false
          else {
            if (this.right.isEmpty && tree.right.isEmpty) true
            else this.right.get.equals(tree.left.get)
          }

        l &&
        this.interval.equals(tree.interval) && this.v.equals(tree.v) &&
        r
      }
      case _ => false
    }
  }

  def intervalLength(implicit g: Group[K]): K = {
    g.remove(upper, lower)
  }

  override def lowerBound: InclusiveLower[K] = {
    left match {
      case None => {
        interval.lower
      }
      case Some(i @ InternalNode(_, _, _, _)) => {
        i.lowerBound
      }
      case Some(Leaf(Intersection(bound: InclusiveLower[K], _), _)) => {
        bound
      }
    }
  }

  override def upperBound: Upper[K] = {
    right match {
      case None => {
        interval.upper
      }
      case Some(i @ InternalNode(_, _, _, _)) => {
        i.upperBound
      }
      case Some(Leaf(Intersection(_, bound: ExclusiveUpper[K]), _)) => {
        bound
      }
    }
  }

  def lower: K = lowerBound.lower

  def upper: K = upperBound match{
    case i: InclusiveUpper[K] => i.upper
    case i: InclusiveLower[K] => i.lower
  }

  def sumBy[T: Monoid](low: K, upp: K, f: (K, K, V) => T): T = {
    val m = implicitly[Monoid[T]]
    if (interval.contains(low) && interval.contains(upp)) f(low, upp, v)
    else if (interval.contains(low)) {
      val thisVal = f(low, interval.upper.upper, v)
      if (right.nonEmpty) m.plus(thisVal, right.get.sumBy(low, upp, f))
      else thisVal
    }
    else if (interval.contains(upp)) {
      val thisVal = f(interval.lower.lower, upp, v)
      if (left.nonEmpty) m.plus(thisVal, left.get.sumBy(low, upp, f))
      else thisVal
    }
    else {
      val thisVal = f(interval.lower.lower, interval.upper.upper, v)
      val l =
        if (left.nonEmpty) left.get.sumBy(low, upp, f)
        else m.zero
      val r =
        if (right.nonEmpty) right.get.sumBy(low, upp, f)
        else m.zero
      m.plus(m.plus(l, thisVal), r)
    }
  }

}

final case class InternalNode[K: Ordering, +V](
                                              override val interval: InLowExUp[K],
                                              override val v: V,
                                              left: IntervalTree[K, V],
                                              right: IntervalTree[K, V])
extends AbstractInternalNode(interval, v, left, right) {


  override def tuple: (Intersection[InclusiveLower, ExclusiveUpper, K], V) = (interval, v)

  override lazy val hasBoundNode: Boolean = right.hasBoundNode

  override def upp: K = interval.upper.upper

  def sliceLower(x: K): IntervalTree[K, V] = {
    x match{
      case c if interval.contains(x) => {
        new InternalNode[K, V](
          Interv.unsafeLeftClosedRightOpen(x, upp), v, EmptyNode[K, V], right
        )
      }
      case l if this.upperThan(x) => {
        new InternalNode[K, V](
          interval, v, left.sliceLower(x), right
        )
      }
      case r if this.lowerThan(x) => {
        right.sliceLower(x)
      }
    }
  }


}

final case class UpperBoundInternalNode[K: Ordering, V](override val interval: InLowInUp[K],
                                                        override val v: V,
                                                        left: NonEmptyIntervalTree[K, V])
  extends AbstractInternalNode(interval, v, left, EmptyNode[K, V]()) with BoundNode[K, V]{

  override
  def tuple: (Intersection[InclusiveLower, InclusiveUpper, K], V) = (interval, v)

  override lazy val hasBoundNode: Boolean = true

  def sliceLower(x: K): IntervalTree[K, V] = {
    x match{
      case c if interval.contains(x) => {
        new UpperBoundLeaf[K, V](
          Interv.closed(x, upp), v)
      }
      case l if this.upperThan(x) => {
        left.sliceLower(x) match {
          case leaf: Leaf[K, V] => {
            new UpperBoundInternalNode[K, V](
              interval, v, leaf)
          }
          case internal: InternalNode[K, V] =>  {
            new UpperBoundInternalNode[K, V](
              interval, v, internal)
          }
          case empty: EmptyNode[K, V] => {
            new UpperBoundLeaf[K, V](
              Interv.closed(low, upp), v)
          }
        }
      }
      case r if this.lowerThan(x) => {
        this
      }
    }
  }
}

abstract class AbstractLeaf[K: Ordering, V](
                            override val interval: Intersection[InclusiveLower, Upper, K],
                            override val v: V)
  extends NonEmptyIntervalTree[K, V](interval, v){

  override lazy val size = 1

  protected def cons[V1](low: K, upp: K, v: V1): AbstractLeaf[K, V1]

  protected def map[V1 >: V, V2](f: (K, K, V1) =>
    (K, K, V2))(implicit p: Predecessible[K], s: Successible[K]): IntervalTree[K, V2] = {
    val optRes =
    for{
      low <- interval.lower.strictLowerBound
      upp <- interval.upper.strictUpperBound
    } yield {
      val (newLow, newUpp, newV) = f(low, upp, v)
      if (implicitly[Ordering[K]].lt(newLow, newUpp)) {
        cons(newLow, newUpp, newV)
      }
      else EmptyNode[K, V2]()
    }
    optRes.getOrElse(EmptyNode[K, V2]())
  }

  override def sliceLower(x: K): IntervalTree[K, V] = {
    val upp = IntervalTree.extUpper(interval)
    if (interval.contains(x)) cons(x, upp, v)
    else this
  }

  override lazy val size: Int = 1


  import scala.collection._
  def collect[T](pf: PartialFunction[(InLowExUp[K], V), T])(
    implicit bi: mutable.Builder[T, Iterator[T]]): mutable.Builder[T, Iterator[T]] = {
    if(pf.isDefinedAt((interval, v))) bi += pf((interval, v))
    bi
  }

  override def mapNonDecreasing[U: Ordering](i: (K) => U): Leaf[U, V] = {
    interval.mapNonDecreasing(i) match {
      case u: Universe[U] => ???
      case empty: Empty[U] => ???
      case i: InLowVarUp[U] => {
        new Leaf[U, V](i, v)
      }
    }
  }

  override def lowerBound: InclusiveLower[K] = interval.lower

  override def upperBound: Upper[K] = interval.upper

  override def map[V1](f: (V) => V1): Leaf[K, V1] = {
    this.copy(v = f(v))
  }

  override def map[U: Ordering, V1](f: (InLowExUp[K], V) =>
    (InLowExUp[U], V1)): Leaf[U, V1] = {
    new Leaf[U, V1](f(interval, v))
  }

  override def map[V1](f: (K, K, V) => V1): Leaf[K, V1] = {
    val low = interval.lower.lower
    val upp = interval.upper match{
      case InclusiveUpper(x) => x
      case ExclusiveUpper(x) => x
    }
    new Leaf[K, V1](interval, f(low, upp, v))
  }

  override
  def mapIterator[U: Ordering, V1](
              f: (InLowExUp[K], V) => (Iterator[(InLowExUp[U], V1)], Int)
              ): (Iterator[(InLowExUp[U], V1)], Int) = {
    f(interval, v)
  }


  def buildIterator[V1 >: V](array: Array[(InLowVarUp[K], V1)], start: Int)
  : Array[(InLowVarUp[K], V1)] = {
    array.update(start, tuple)
    array
  }

  def iterator: Iterator[(InLowVarUp[K, U], V)] = Iterator.single(tuple)

  override def equals(obj: scala.Any): Boolean = {
    obj match{
    case leaf: Leaf[Any, Any] => {
      this.interval.equals(leaf.interval) && this.v.equals(leaf.v)
    }
    case _ => false
  }}

  override def toString =
    s"[${interval.lower.lower}, ${interval.upper.upper}):" +
      s" ${v.toString}" + System.lineSeparator()

  def intervalLength(implicit g: Group[K]): K = {
    g.remove(NonEmptyIntervalTree.extUpper(interval.upper), interval.lower.lower)
  }

  override def sumBy[T: Monoid](low: K, upp: K, f: (K, K, V) => T): T = {
    if (interval.contains(low) && interval.contains(upp)) f(low, upp, v)
    else if (interval.contains(low)) f(low, NonEmptyIntervalTree.extUpper(interval.upper), v)
    else if (interval.contains(upp)) f(interval.lower.lower, upp, v)
    else if (interval.lower.contains(upp) && interval.upper.contains(low))
      f(interval.lower.lower, NonEmptyIntervalTree.extUpper(interval.upper), v)
    else implicitly[Monoid[T]].zero
  }

}

final case class Leaf[K: Ordering, V](
                                     override val interval: Intersection[InclusiveLower, ExclusiveUpper, K],
                                     override val v: V
                                     ) extends AbstractLeaf[K, V](interval, v){

  override lazy val hasBoundNode: Boolean = false

  protected def cons[V1](low: K, upp: K, v: V1): Leaf[K, V1] = {
    new Leaf(Interv.unsafeLeftClosedRightOpen(low, upp), v)
  }

  override def sliceUpper(x: K): IntervalTree[K, V] = {
    val low = interval.lower.lower
    if (interval.contains(x)) {
      new UpperBoundLeaf(Interv.closed(low, x), v)
    }
    else this
  }


}
final case class UpperBoundLeaf[K: Ordering, V](
                                          override val interval: InLowInUp[K],
                                          override val v: V
                                          )
  extends AbstractLeaf[K, V, InclusiveUpper[K]](interval, v) with BoundNode[K, V]{

  override lazy val hasBoundNode: Boolean = true

  protected def cons[V1](low: K, upp: K, v: V1): UpperBoundLeaf[K, V1] = {
    new UpperBoundLeaf(Interv.closed(low, upp), v)
  }


  override def sliceUpper(x: K): IntervalTree[K, V] = {
    val low = interval.lower.lower
    if (interval.contains(x)) cons(low, x, v)
    else this
  }


}

trait BoundNode[K, V] extends NonEmptyIntervalTree[K, V, InclusiveUpper[K]]

object IntervalTree{

  object Interv{
    def unsafeLeftClosedRightOpen[K](low: K, upp: K)(
      implicit ord: Ordering[K]): InLowExUp[K] = {
      assert(ord.lt(low, upp))
      new Intersection(InclusiveLower(low), ExclusiveUpper(upp))
    }
    def closed[K: Ordering](low: K, upp: K)(
      implicit ord: Ordering[K]): InLowInUp[K] = {
      assert(ord.lt(low, upp))
      new Intersection(InclusiveLower(low), InclusiveUpper(upp))
    }
  }

  private[intervaltree]
  def extUpper[K, U <: Upper[K], L <: Lower[K]](i: Intersection[U, L, K]): K =
    i.upper match {
      case inc: InclusiveUpper[K] => inc.upper
      case exc: ExclusiveUpper[K] => exc.upper
  }

  private[intervaltree]
  def extLower[K, U <: Upper[K], L <: Lower[K]](i: Intersection[U, L, K]): K =
    i.lower match{
      case inc: InclusiveLower[K] => inc.lower
      case exc: ExclusiveLower[K] => exc.lower
  }

  private[intervaltree]
  def copyUpper[K, U <: Upper[K]](i: Intersection[InclusiveLower, U, K],
                                  upp: K): Intersection[InclusiveLower, Upper[K], K] =
    copyInterval(i, i.lower.lower, upp)

  private[intervaltree]
  def copyInterval[K, L <: Lower[K], U <: Upper[K]](
                   i: Intersection[L, U, K],
                   low: K, upp: K): Intersection[L, Upper[K], K] =
    i.upper match{
      case inc: InclusiveUpper[K] => {
        new Intersection(new InclusiveLower[K](low), new InclusiveUpper[K](upp))
      }
      case exc: ExclusiveUpper[K] => {
        new Intersection(new InclusiveLower[K](low), new ExclusiveUpper[K](upp))
      }
  }

  type InLowVarUp[K, U <: Upper[K]] = Intersection[InclusiveLower, U, K]

  def buildOne[K: Ordering, V](xLow: K, xUpp: K, f: V
                              ): Option[UpperBoundLeaf[K, V]]= {
    val ord = implicitly[Ordering[K]]
    if (ord.lt(xLow, xUpp)) {
      val interval = new Intersection(InclusiveLower(xLow), InclusiveUpper(xLow))
      Some(new UpperBoundLeaf[K, V](interval, f))
    }
    else None
  }

  implicit def castVarUpperInterval[K, U1 <: Upper[K], U2 >: U1](
                                     inLowVarUp: InLowVarUp[K, U1]
                                     ): InLowVarUp[K, U2] = {
    inLowVarUp.asInstanceOf[InLowVarUp[K, U2]]
  }



  @tailrec
  def find[K: Ordering, V](x: K, tree: Option[NonEmptyIntervalTree[K, V]])
  : Option[NonEmptyIntervalTree[K, V]] = {
    if(tree.nonEmpty) tree.get match{
      case internal: InternalNode[K, V] =>{
        x match{
          case center if internal.interval.contains(center) => tree
          case left if internal.left.nonEmpty &&
                       internal.upperThan(left) =>
            find(x, internal.left)
          case right if internal.right.nonEmpty &&
                        internal.lowerThan(right) =>
            find(x, internal.right)
          case _  => None
        }
      }
      case leaf: Leaf[K, V] => {
        if(leaf.interval.contains(x)) tree else None
    }
  } else None
}

  def apply[K: Ordering, V](vect: List[(InLowVarUp[K], V)])
  : Option[NonEmptyIntervalTree[K, V]] = {
    Trampoline.run(buildLeft(vect))
  }


  @tailrec
  final def findNode[K, V](tree: Option[NonEmptyIntervalTree[K, V]], low: K, upp: K)(
    implicit ord: Ordering[K]): Option[NonEmptyIntervalTree[K, V]] = {
    tree match {
      case Some(InternalNode(
      Intersection(InclusiveLower(lower), ExclusiveUpper(upper)),
      _, left, right)) => {
        if (ord.gt(low, upper)) findNode(right, low, upp)
        else if(ord.lt(upp, lower)) findNode(left, low, upp)
        else tree
      }
      case Some(Leaf(Intersection(InclusiveLower(lower), ExclusiveUpper(upper)), _)) => {
        if (ord.lt(low, upper) && ord.gt(upp, lower)) tree
        else None
      }
      case None => None
    }
  }

  def area[K: Ordering, V](lower: K, upper: K, tree: Option[NonEmptyIntervalTree[K, V]])
  : Trampoline[List[(InLowVarUp[K], V)]] = {
    def contain(interval: InLowVarUp[K]): Boolean = {
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

  def toList[K: Ordering, V](tree: Option[NonEmptyIntervalTree[K, V]],
                             buffer: collection.mutable.ListBuffer[(InLowVarUp[K], V)])
  : Trampoline[collection.mutable.ListBuffer[(InLowVarUp[K], V)]] = {
    tree match{
      case None => Done(buffer)
      case Some(InternalNode(interval, v, left, right)) => {
        for {
          l <- call(toList(left, buffer))
          c <- Done(l.+=((interval, v)))
          r <- call(toList(right, c))
        } yield r
      }
      case Some(Leaf(interval, v)) => Done(buffer.+=((interval, v)))
    }
}


  final def buildLeft[K: Ordering, V](vals: Iterator[(List[K], V)],
                                      size: Int): IntervalTree[K, V] = {

      size match {
        case 1 => {
          val (low :: upp :: Nil, func) = vals.next()
          new Leaf(Interv.unsafeLeftClosedRightOpen(low, upp), func)
        }
        case 2 => {
          val (leftLow :: leftUpp :: Nil, leftFunc) = vals.next()
          val (midLow :: midUpp :: Nil, midFunc) = vals.next()

          val leftI = Interv.unsafeLeftClosedRightOpen(leftLow, leftUpp)
          val midI = Interv.unsafeLeftClosedRightOpen(midLow, midUpp)

          new InternalNode(midI, midFunc, new Leaf(leftI, leftFunc), EmptyNode[K, V]())
        }
        case 3 => {
          val (leftLow :: leftUpp :: Nil, leftFunc) = vals.next()
          val (midLow :: midUpp :: Nil, midFunc) = vals.next()
          val (rightLow :: rightUp :: Nil, rightFunc) = vals.next()

          val leftI = Interv.unsafeLeftClosedRightOpen(leftLow, leftUpp)
          val midI = Interv.unsafeLeftClosedRightOpen(midLow, midUpp)
          val rightI = Interv.unsafeLeftClosedRightOpen(rightLow, rightUp)

          new InternalNode(midI, midFunc,
            new Leaf(leftI, leftFunc), new Leaf(rightI, rightFunc))
        }
        case s => {
          val leftIndex =
            if(s % 2 == 0) (size + 1) / 2
            else s / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val (midLow :: midUpp :: Nil, midFunc) = vals.next()
          val midI = Interv.unsafeLeftClosedRightOpen(midLow, midUpp)
          val right = buildRight(vals, rightSize)
          new InternalNode[K, V](midI, midFunc, left, right)
        }
      }
  }


  final def buildRight[K: Ordering, V](vals: Iterator[(List[K], V)],
                                       size: Int): IntervalTree[K, V] = {
      size match {
        case 1 => {
          val (low :: upp :: Nil, func) = vals.next()
          new UpperBoundLeaf(Interv.closed(low, upp), func)
        }
        case 2 => {

          val (midLow :: midUpp :: Nil, midFunc) = vals.next()
          val (rightLow :: rightUpp :: Nil, rightFunc) = vals.next()

          val midI = Interv.unsafeLeftClosedRightOpen(midLow, midUpp)
          val rightI = Interv.closed(rightLow, rightUpp)

          new InternalNode(midI, midFunc, EmptyNode[K, V](),
                           new UpperBoundLeaf(rightI, rightFunc))
        }
        case 3 => {
          val (leftLow :: leftUpp :: Nil, leftFunc) = vals.next()
          val (midLow :: midUpp :: Nil, midFunc) = vals.next()
          val (rightLow :: rightUp :: Nil, rightFunc) = vals.next()

          val leftI = Interv.unsafeLeftClosedRightOpen(leftLow, leftUpp)
          val midI = Interv.unsafeLeftClosedRightOpen(midLow, midUpp)
          val rightI = Interv.closed(rightLow, rightUp)

          new InternalNode(midI, midFunc,
            new Leaf(leftI, leftFunc), new UpperBoundLeaf(rightI, rightFunc))
        }
        case s => {
          val leftIndex =
            if(size % 2 == 0) (size - 1) / 2
            else size / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val (midLow :: midUpp :: Nil, midFunc) = vals.next()
          val midI = Interv.unsafeLeftClosedRightOpen(midLow, midUpp)
          val right = buildRight(vals, rightSize)
          new InternalNode[K, V](midI, midFunc, left, right)
        }
      }
  }

  def map[K, K1, V, V2](tree: IntervalTree[K, V], f: (K, V) => (K, V2)) = {
    tree match {
      case empty: EmptyNode[K, V] => EmptyNode[K, V2]()
      case internal: AbstractInternalNode[K, V]
    }
  }


}