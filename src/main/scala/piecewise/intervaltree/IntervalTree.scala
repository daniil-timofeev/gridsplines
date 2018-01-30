package piecewise.intervaltree

import com.twitter.algebird.Interval.{InLowExUp, InLowInUp, MaybeEmpty}
import com.twitter.algebird.monad.Trampoline._
import com.twitter.algebird.monad._
import com.twitter.algebird.{Intersection, _}
import piecewise.intervaltree.IntervalTree._

import scala.annotation.tailrec
import scala.collection.Iterator



/**
  *
  */

abstract class IntervalTree[K, +V]{

  def size: Int

  def hasBoundNode: Boolean

  def sliceUpper(x: K): IntervalTree[K, V]

  def sliceLower(x: K): IntervalTree[K, V]

  def upperThan(x: K): Boolean

  def lowerThan(x: K): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  private[intervaltree] def updateArray[V1 >: V](
                              array: Array[((K, K), V1)],
                              pos: Int): Array[((K, K), V1)]

  def array[V1 >: V](implicit ord: Ordering[K]): Array[((K, K), V1)] = {
    this match {
      case empty: EmptyNode[K, V] => Array.empty[((K, K), V1)]
      case InternalNode(_, _, left, _) => {
        updateArray(new Array[((K, K), V1)](size), left.size)
      }
      case UpperBoundInternalNode(_, _, left) => {
        updateArray(new Array[((K, K), V1)](size), left.size)
      }
      case UpperBoundLeaf(i, v) => Array((ext(i), v))
      case Leaf(i, v) => Array((ext(i), v))
    }
  }

  def iterator(implicit ord: Ordering[K]): Iterator[((K, K), V)] = {
    array.iterator
  }

  def rebuild(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    this match {
      case empty: EmptyNode[K, V] => empty
      case nonEmpty: NonEmptyITree[K, V, Upper] => {
        IntervalTree.buildRight(nonEmpty.iterator, size)
      }
    }
  }

}

case class EmptyNode[K, +V]()extends IntervalTree[K, V]{

  val size = 0

  override def hasBoundNode = false

  override def sliceUpper(x: K) = this

  override def sliceLower(x: K) = this

  override def upperThan(x: K) = true

  override def lowerThan(x: K) = true

  override def equals(that: Any) = that match{
    case empty: EmptyNode[K, V] => true
    case other => false
  }

  override val isEmpty: Boolean = true

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int): Array[((K, K), V1)] = {
    array
  }

  override def toString: String = ""
}

abstract class NonEmptyITree[K, +V, U[t] <: Upper[t]](
  val interval: Intersection[InclusiveLower, U, K],
  val v: V)
extends IntervalTree[K, V]{

  implicit val ord: Ordering[K]

  def contains(x: K): Boolean = interval.contains(x)

  def apply: V = v

  def map[K2: Ordering, V1 >: V, V2](f: (K, K, V1) => (K2, K2, V2)
                                    ): IntervalTree[K2, V2] = {
    Trampoline.run(IntervalTree.map(this, f))
  }

  override
  def upperThan(x: K): Boolean = !interval.lower.contains(x)

  override
  def lowerThan(x: K): Boolean = !interval.upper.contains(x)

  def tuple: (Intersection[InclusiveLower, U, K], V) = (interval, v)

  def low: K = interval.lower.lower
  def upp: K = IntervalTree.extUpper(interval)

  def intervalLength(implicit G: Group[K]): K = {
    G.minus(upp, low)
  }

  def upperBound: InclusiveUpper[K]

  def lowerBound: InclusiveLower[K]

  def wholeInterval: InLowInUp[K] = {
    {lowerBound && upperBound} match {
      case expected: InLowInUp[K] => expected
      case _  => throw new RuntimeException("Non empty tree must have not empty interval")
    }
  }


  override val isEmpty: Boolean = false

}

abstract class AbstractInternalNode[K, +V, U[t] <: Upper[t]](
                          override val interval: Intersection[InclusiveLower, U, K],
                          override val v: V,
                          val left: IntervalTree[K, V],
                          val right: IntervalTree[K, V])
  extends NonEmptyITree[K, V, U](interval, v){

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case tree: InternalNode[Any, Any] => {
        val l =
          if (this.left.isEmpty && tree.left.nonEmpty ||
            this.left.nonEmpty && tree.left.isEmpty) false
          else {
            if (this.left.isEmpty && tree.left.isEmpty) true
            else this.left.equals(tree.left)
          }

        val r =
          if (this.right.isEmpty && tree.right.nonEmpty ||
            this.right.nonEmpty && tree.right.isEmpty) false
          else {
            if (this.right.isEmpty && tree.right.isEmpty) true
            else this.right.equals(tree.left)
          }

        l &&
        this.interval.equals(tree.interval) && this.v.equals(tree.v) &&
        r
      }
      case _ => false
    }
  }

  override def lowerBound: InclusiveLower[K] = {
    left match {
      case empty: EmptyNode[K, V] => interval.lower
      case node @ InternalNode(_, _, _, _) => node.lowerBound
      case node @ UpperBoundInternalNode(_, _, _) => node.lowerBound
      case Leaf(i, _) => i.lower
      case UpperBoundLeaf(i, _) => i.lower
    }
  }

  override def upperBound: InclusiveUpper[K] = {
    right match {
      case node @ InternalNode(_, _, _, _) => node.upperBound
      case UpperBoundInternalNode(i, _, _) => i.upper
      case UpperBoundLeaf(i, _) => i.upper
      case empty: EmptyNode[K, V] =>
        throw new RuntimeException("right cannot be empty there")
      case leaf: Leaf[K, V] =>
        throw new RuntimeException("right cannot be leaf there")
    }
  }

}

final case class InternalNode[K, +V](override val interval: InLowExUp[K],
                                     override val v: V,
                                     override val left: IntervalTree[K, V],
                                     override val right: IntervalTree[K, V])
                                    (implicit val ord: Ordering[K])
extends AbstractInternalNode(interval, v, left, right) {


  override def tuple: (Intersection[InclusiveLower, ExclusiveUpper, K], V) = (interval, v)

  override lazy val hasBoundNode: Boolean = right.hasBoundNode

  override def upp: K = interval.upper.upper

  override lazy val size: Int = 1 + left.size + right.size

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)],
                           pos: Int): Array[((K, K), V1)] = {
    left.updateArray(array, posOfLeft(left, pos))
    array.update(pos, (ext(interval), v))
    right.updateArray(array, posOfRight(right, pos))
  }

  override def toString: String = {
      left.toString + System.lineSeparator() +
      IntervalTree.show(interval) + " : " + v.toString + ";" +
      System.lineSeparator() + right.toString
  }

  def sliceLower(x: K): IntervalTree[K, V] = {
    x match{
      case c if interval.contains(x) => {
        new InternalNode[K, V](
          Interv.unsafeInLowExUp(x, upp), v, EmptyNode[K, V], right
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

  def sliceUpper(x: K): IntervalTree[K, V] = {
    x match {
      case c if interval.contains(c) => {
        val lower = IntervalTree.extLower(interval)
        buildUpperBound(lower, c, v, left)
      }
      case l if upperThan(x) => {
        left.sliceUpper(x)
      }
      case r if lowerThan(x) => {
        right.sliceUpper(x) match{
          case empty: EmptyNode[K, V] => {
            val (lower, upper) = IntervalTree.ext(interval)
            buildUpperBound(lower, upper, v, left)
          }
          case nonEmpty => new InternalNode(interval, v, left, nonEmpty)
        }
      }
    }
  }

}

final case class UpperBoundInternalNode[K, V](override val interval: InLowInUp[K],
                                              override val v: V,
                                              override val left: NonEmptyITree[K, V, ExclusiveUpper])
                                             (implicit val ord: Ordering[K])
  extends AbstractInternalNode(interval, v, left, EmptyNode[K, V]()) with BoundNode[K, V]{

  override
  def tuple: (Intersection[InclusiveLower, InclusiveUpper, K], V) = (interval, v)

  override val size: Int = 1 + left.size

  override lazy val hasBoundNode: Boolean = true

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int): Array[((K, K), V1)] = {
    left.updateArray(array, posOfLeft(left, pos))
    array.update(pos, (ext(interval), v))
    array
  }

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

  override def upperBound: InclusiveUpper[K] = interval.upper

  override def toString: String = {
    left.toString + System.lineSeparator() +
    IntervalTree.show(interval) + " : " + v.toString + ";"
  }

  def sliceUpper(x: K): IntervalTree[K, V] = {
    x match {
      case c if interval.contains(c) => {
        val (lower, upper) = IntervalTree.ext(interval)
        buildUpperBound(low, c, v, left)
      }
      case l if upperThan(x) => {
        left.sliceUpper(x)
      }
      case r if lowerThan(x) => {
        this
      }
    }
  }

}


abstract class AbstractLeaf[K: Ordering, +V, U[t] <: Upper[t]](
                            override val interval: Intersection[InclusiveLower, U, K],
                            override val v: V)
  extends NonEmptyITree[K, V, U](interval, v){

  override def equals(obj: scala.Any): Boolean = {
    obj match {
    case leaf: Leaf[Any, Any] => {
      this.interval.equals(leaf.interval) && this.v.equals(leaf.v)
    }
    case _ => false
  }}

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int
                          ): Array[((K, K), V1)] = {
    array.update(pos, (ext(interval), v))
    array
  }

  override def lowerBound: InclusiveLower[K] = interval.lower

}

final case class Leaf[K, +V](
        override val interval: Intersection[InclusiveLower, ExclusiveUpper, K],
        override val v: V
        )(implicit val ord: Ordering[K])
  extends AbstractLeaf[K, V, ExclusiveUpper](interval, v){

  def this(low: K, upp: K, v: V)(implicit ord: Ordering[K]){
    this(Interv.unsafeInLowExUp(low, upp), v)
  }
  override lazy val hasBoundNode: Boolean = false

  override val size: Int = 1

  override def upperBound: InclusiveUpper[K] = ???

  override def sliceLower(x: K): IntervalTree[K, V] = {
    val upp = interval.upper.upper
    if (interval.contains(x)) {
      new Leaf(x, upp, v)
    }
    else this
  }

  override def sliceUpper(x: K): IntervalTree[K, V] = {
    val low = interval.lower.lower
    if (interval.contains(x)) {
      IntervalTree.foldToIntTree(
        IntervalTree.closed(low, x),
        (i: InLowInUp[K]) => new UpperBoundLeaf[K, V](i, v),
        () => EmptyNode[K, V]
      )
    }
    else this
  }

  override def toString: String = {
    IntervalTree.show(interval) + " : " + v.toString + ";"
  }

}
final case class UpperBoundLeaf[K, +V](
                                  override val interval: InLowInUp[K],
                                  override val v: V
                                  )(implicit val ord: Ordering[K])
  extends AbstractLeaf[K, V, InclusiveUpper](interval, v) with BoundNode[K, V]{

  def this(low: K, upp: K, v: V)(implicit ord: Ordering[K]){
    this(Interv.closed(low, upp), v)
  }

  override lazy val hasBoundNode: Boolean = true

  override val size: Int = 1

  override def sliceLower(x: K): IntervalTree[K, V] = {
    val upp = interval.upper.upper
    if (interval.contains(x)) {
      new UpperBoundLeaf(x, upp, v)
    }
    else this
  }

  override def sliceUpper(x: K): IntervalTree[K, V] = {
    val low = interval.lower.lower
    if (interval.contains(x)) {
      IntervalTree.foldToIntTree(
        IntervalTree.closed(low, x),
        (i: InLowInUp[K]) => new UpperBoundLeaf[K, V](i, v),
        () => EmptyNode[K, V]
      )
    }
    else this
  }

  override def upperBound: InclusiveUpper[K] = interval.upper

  override def toString: String = {
    IntervalTree.show(interval) + " : " + v.toString + ";"
  }

}

trait BoundNode[K, +V] extends NonEmptyITree[K, V, InclusiveUpper]{
  val interval: InLowInUp[K]
  val v: V
  def upperBound: InclusiveUpper[K] = interval.upper
}

object IntervalTree{

  object Interv{
    def unsafeInLowExUp[K](low: K, upp: K)(
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
  def closed[K: Ordering](lower: K,
                          upper: K): MaybeEmpty[K, InLowInUp] = {
    if (implicitly[Ordering[K]].lt(lower, upper))
      MaybeEmpty.NotSoEmpty[K, InLowInUp](
        Intersection(InclusiveLower(lower), InclusiveUpper(upper)))
    else MaybeEmpty.SoEmpty[K, InLowInUp]
  }

  private[intervaltree]
  def foldToIntTree[K: Ordering, V, Intrvl[k] <: Interval[k]](
        interval: MaybeEmpty[K, Intrvl],
        ifNotEmptyFunc: Intrvl[K] => IntervalTree[K, V],
        empty: () => IntervalTree[K, V]): IntervalTree[K, V] = {
    interval match{
      case notEmpty: MaybeEmpty.NotSoEmpty[K, Intrvl] => ifNotEmptyFunc(notEmpty.get)
      case _ => empty()
    }
  }

  private[intervaltree]
  def convertMostRight[K: Ordering, V](tree: IntervalTree[K, V]): IntervalTree[K, V] = {
    tree match {
      case empty: EmptyNode[K, V] => empty
      case InternalNode(i, v, left, right) => {
        right match{
          case empty: EmptyNode[K, V] => {
            left match {
              case empty: EmptyNode[K, V] => new UpperBoundLeaf(i, v)
              case nonEmpty: NonEmptyITree[K, V, ExclusiveUpper] =>
                new UpperBoundInternalNode(i, v, nonEmpty)
            }
          }
          case nonEmpty: NonEmptyITree[K, V, ExclusiveUpper] => {
              new InternalNode(i, v, left, convertMostRight(right))
            }
          }
      }
      case Leaf(i, v) => UpperBoundLeaf(i, v)
      case upperLeaf: UpperBoundLeaf[K, V] => upperLeaf
    }
  }

  private[intervaltree]
  def buildUpperBound[K: Ordering, V](low: K,
                                      upp: K,
                                      v: V,
                                      left: IntervalTree[K, V]): IntervalTree[K, V] = {
    left match{
      case empty: EmptyNode[K, V] => {
        IntervalTree.foldToIntTree(
          IntervalTree.closed(low, upp),
          (i: InLowInUp[K]) => new UpperBoundLeaf[K, V](i, v),
          () => EmptyNode[K, V]())
      }
      case nonEmpty: NonEmptyITree[K, V, ExclusiveUpper] => {
        IntervalTree.foldToIntTree(
          IntervalTree.closed(low, upp),
          (i: InLowInUp[K]) => new UpperBoundInternalNode[K, V](i, v, nonEmpty),
          () => convertMostRight(nonEmpty)
        )
      }
      }
  }



  private[intervaltree]
  def show[K, L[k] <: Lower[k], U[k] <: Upper[k]](
          interval: Intersection[L, U, K]): String = {
    interval match {
      case Intersection(InclusiveLower(low: K), ExclusiveUpper(upp: K)) =>
        f"[${low}, ${upp})"
      case Intersection(InclusiveLower(low: K), InclusiveUpper(upp: K)) =>
        f"[${low}, ${upp}]"
    }
  }

  private[intervaltree]
  def extUpper[K,  L[k] <: Lower[k], U[k] <: Upper[k]](i: Intersection[L, U, K]): K =
  i.upper match {
    case inc: InclusiveUpper[K] => inc.upper
    case exc: ExclusiveUpper[K] => exc.upper
  }

  private[intervaltree]
  def extLower[K,  L[k] <: Lower[k], U[k] <: Upper[k]](i: Intersection[L, U, K]): K =
  i.lower match{
    case inc: InclusiveLower[K] => inc.lower
    case exc: ExclusiveLower[K] => exc.lower
  }

  private[intervaltree]
  def ext[K, L[k] <: Lower[k], U[k] <: Upper[k]](i: Intersection[L, U, K])
    : (K, K) = {
    (extLower(i), extUpper(i))
  }

  private[intervaltree]
  def posOfLeft[K: Ordering, V](tree: IntervalTree[K, V], parentPos: Int): Int = {
    tree match {
      case empty: EmptyNode[K, V] => parentPos - 1
      case InternalNode(
        interval: InLowExUp[K],
        v: V,
        _,
        right) => {
        parentPos - right.size - 1
      }
      case UpperBoundInternalNode(_, _, left) => parentPos - 1
      case l: Leaf[K, V] => parentPos - 1
      case ul: UpperBoundLeaf[K, V] => {
        parentPos - 1
      }
    }
  }

  private[intervaltree]
  def posOfRight[K: Ordering, V](tree: IntervalTree[K, V], parentPos: Int): Int = {
    tree match {
      case empty: EmptyNode[K, V] => parentPos + 1
      case InternalNode(
        interval: InLowExUp[K],
        v: V,
        left,
        _) => {
        parentPos + left.size + 1
      }
      case UpperBoundInternalNode(_, _, left) => parentPos + 1
      case l: Leaf[K, V] => parentPos + 1
      case ul: UpperBoundLeaf[K, V] => {
        parentPos + 1
      }
    }
  }



  private[intervaltree]
  def copyInterval[L[k] <: Lower[k], U[k] <: Upper[k], K](
                   i: Intersection[L, U, K],
                   low: K, upp: K): Intersection[L, Upper, K] =
    i.upper match{
      case inc: InclusiveUpper[K] => {
        {new Intersection(new InclusiveLower[K](low),
          new InclusiveUpper[K](upp))}.asInstanceOf[Intersection[L, Upper, K]]
      }
      case exc: ExclusiveUpper[K] => {
        {new Intersection(new InclusiveLower[K](low),
          new ExclusiveUpper[K](upp))}.asInstanceOf[Intersection[L, Upper, K]]
      }
  }

  type InLowVarUp[K, U[t] <: Upper[t]] = Intersection[InclusiveLower, U, K]

  def buildOne[K: Ordering, V](xLow: K, xUpp: K, f: V
                              ): IntervalTree[K, V] = {
    val ord = implicitly[Ordering[K]]
    if (ord.lt(xLow, xUpp)) {
      val interval = new Intersection(InclusiveLower(xLow), InclusiveUpper(xUpp))
      new UpperBoundLeaf[K, V](interval, f)
    }
    else EmptyNode[K, V]()
  }


  def valueAt[K: Ordering, V](x: K, tree: IntervalTree[K, V]): Option[V] = {
      find(x, tree) match{
        case empty: EmptyNode[K, V] => None
        case nonEmpty: NonEmptyITree[K, V, Upper] => Some(nonEmpty.v)
      }
  }

  def unsafeValueAt[K, V](x: K, tree: IntervalTree[K, V]): V = {
    find(x, tree) match {
      case empty: EmptyNode[K, V] => ???
      case nonEmpty: NonEmptyITree[K, V, Upper] => nonEmpty.v
    }
  }

  @tailrec
  def find[K, V](x: K, tree: IntervalTree[K, V]): IntervalTree[K, V] = {
    tree match {
      case empty: EmptyNode[K, V] => empty
      case internal: InternalNode[K, V] => {
        if (internal.contains(x)) internal
        else if (internal.upperThan(x)) find(x, internal.left)
        else find(x, internal.right)
      }
      case upperInternal: UpperBoundInternalNode[K, V] => {
        if (upperInternal.contains(x)) upperInternal
        else if (upperInternal.upperThan(x)) find(x, upperInternal.left)
        else EmptyNode[K, V]
      }
      case leaf: Leaf[K, V] => {
        if (leaf.contains(x)) leaf
        else EmptyNode[K, V]
      }
      case leaf: UpperBoundLeaf[K, V] => {
        if (leaf.contains(x)) leaf
        else EmptyNode[K, V]
      }
    }
}



  final def extract[K: Ordering, V](tree: IntervalTree[K, V]): ((K, K), V) = {
    tree match {
      case InternalNode(
        interval: InLowExUp[K], v: V, _, _) => {
        (ext(interval), v)
      }
      case UpperBoundInternalNode(interval: InLowInUp[K], v: V, _) => {
        (ext(interval), v)
      }
      case Leaf(interval: InLowExUp[K], v: V) => {
        (ext(interval), v)
      }
      case UpperBoundLeaf(interval: InLowInUp[K], v: V) => {
        (ext(interval), v)
      }
      case empty: EmptyNode[K, V] => ???
    }
  }


  def map[K, V, V1 >: V, K2: Ordering, V2](tree: IntervalTree[K, V],
                                 f: (K, K, V1) => (K2, K2, V2)
                                ): Trampoline[IntervalTree[K2, V2]] = {
    tree match {
      case InternalNode(
      interval: InLowExUp[K],
      v1: V1, left, right) => {
        val (low: K2, upp: K2, v2: V2) = f(extLower(interval), extUpper(interval), v1)
        for{
          left1 <- call(map(left, f))
          right1 <- call(map(right, f))
        } yield {
          new InternalNode(
            Interv.unsafeInLowExUp(low, upp), v2, left1, right1)
        }
      }
       case UpperBoundInternalNode(
          interval: InLowInUp[K],
          v1: V1, left) =>
        map(left, f).map{left => {
          val (low: K2, upp: K2, v2: V2) =
            f(extLower(interval), extUpper(interval), v1)

          left match {
            case empty: EmptyNode[K2, V2] => empty
            case internal: InternalNode[K2, V2] => {
              new UpperBoundInternalNode[K2, V2](Interv.closed(low, upp), v2, internal)
            }
            case nonEmpty: NonEmptyITree[K2, V2, ExclusiveUpper] =>
              new UpperBoundInternalNode[K2, V2](Interv.closed(low, upp), v2, nonEmpty)
          }
        }
      }
      case Leaf(interval, v) => {
        val (low: K2, upp: K2, v2: V2) = f(extLower(interval), extUpper(interval), v)
        Done(new Leaf(Interv.unsafeInLowExUp(low, upp), v2))
      }
      case UpperBoundLeaf(interval, v) => {
        val (low: K2, upp: K2, v2: V2) = f(extLower[K, InclusiveLower, InclusiveUpper](interval), extUpper(interval), v)
        Done(new UpperBoundLeaf(Interv.closed(low, upp), v2))
      }
    }
  }


  final def buildLeft[K: Ordering, V](vals: Iterator[((K, K), V)], size: Int): IntervalTree[K, V] = {

      size match {
        case 1 => {
          val ((low, upp), func) = vals.next()
          new Leaf(Interv.unsafeInLowExUp(low, upp), func)
        }
        case 2 => {
          val ((leftLow, leftUpp), leftFunc) = vals.next()
          val ((midLow, midUpp), midFunc) = vals.next()

          val leftI = Interv.unsafeInLowExUp(leftLow, leftUpp)
          val midI = Interv.unsafeInLowExUp(midLow, midUpp)

          new InternalNode(midI, midFunc, new Leaf(leftI, leftFunc), EmptyNode[K, V]())
        }
        case 3 => {
          val ((leftLow, leftUpp), leftFunc) = vals.next()
          val ((midLow, midUpp), midFunc) = vals.next()
          val ((rightLow, rightUp), rightFunc) = vals.next()

          val leftI = Interv.unsafeInLowExUp(leftLow, leftUpp)
          val midI = Interv.unsafeInLowExUp(midLow, midUpp)
          val rightI = Interv.unsafeInLowExUp(rightLow, rightUp)

          new InternalNode(midI, midFunc,
            new Leaf(leftI, leftFunc),
            new Leaf(rightI, rightFunc)
          )
        }
        case s => {
          val leftIndex =
            if(s % 2 == 0) (size + 1) / 2
            else s / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val((midLow, midUpp), midFunc) = vals.next()
          val midI = Interv.unsafeInLowExUp(midLow, midUpp)
          val right = buildRight(vals, rightSize)
          new InternalNode[K, V](midI, midFunc, left, right)
        }
      }
  }


  final def buildRight[K: Ordering, V](vals: Iterator[((K, K), V)],
                                       size: Int): IntervalTree[K, V] = {
      size match {
        case 0 => {
          EmptyNode[K, V]
        }
        case 1 => {
          val ((low, upp), func) = vals.next()
          if (vals.hasNext) new Leaf(Interv.unsafeInLowExUp(low, upp), func)
          else new UpperBoundLeaf(Interv.closed(low, upp), func)
        }
        case 2 => {
          val ((midLow, midUpp), midFunc) = vals.next()
          val midI = Interv.unsafeInLowExUp(midLow, midUpp)

          new InternalNode(midI, midFunc, EmptyNode[K, V](), buildRight(vals, 1))
        }
        case 3 => {
          val ((leftLow, leftUpp), leftFunc) = vals.next()
          val ((midLow, midUpp), midFunc) = vals.next()

          val leftI = Interv.unsafeInLowExUp(leftLow, leftUpp)
          val midI = Interv.unsafeInLowExUp(midLow, midUpp)

          new InternalNode(midI, midFunc,
            new Leaf(leftI, leftFunc), buildRight(vals, 1))
        }

        case s => {
          val leftIndex =
            if(size % 2 == 0) (size - 1) / 2
            else size / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val ((midLow, midUpp), midFunc) = vals.next()
          val midI = Interv.unsafeInLowExUp(midLow, midUpp)
          val right = buildRight(vals, rightSize)
          new InternalNode[K, V](midI, midFunc, left, right)
        }
      }
  }

  final def subIntervalFold[K, V, R](
         tree: IntervalTree[K, V],
         low: K,
         upp: K,
         f: Function3[K, K, V, R]
    )(implicit R: Monoid[R]): R = {
      tree match {
        case i @ InternalNode(_, v, left, right) => {

          implicit val ord: Ordering[K] = i.ord

          val l =
            if (i.upperThan(low)) subIntervalFold(left, low, i.low, f)
            else R.zero

          val r =
            if (i.lowerThan(upp)) subIntervalFold(right, i.upp, upp, f)
            else R.zero

          val c = f(
              ord.max(low, i.low),
              ord.min(upp, i.upp),
              v
          )

          R.plus(l, R.plus(c, r))
        }
        case i @ UpperBoundInternalNode(_, v, left) => {

          val l =
            if (i.upperThan(low)) subIntervalFold(left, low, i.low, f)
            else R.zero

          val c = f(
            i.ord.max(low, i.low),
            i.ord.min(upp, i.upp),
            v
          )

          R.plus(l, c)
        }
        case l @ Leaf(_, v) => {

          f(
            l.ord.max(l.low, low),
            l.ord.min(l.upp, upp),
            v
          )
        }
        case l @ UpperBoundLeaf(_, v) => {

          f(
            l.ord.max(l.low, low),
            l.ord.min(l.upp, upp),
            v
          )
        }
        case e: EmptyNode[K, V] => R.zero

      }
  }


}