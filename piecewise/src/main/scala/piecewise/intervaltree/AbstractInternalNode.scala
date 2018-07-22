package piecewise.intervaltree

import piecewise.intervaltree.IntervalTree._

import scala.language.higherKinds


/** Internal node can be placed at most right position.
  * In that case it contain closed [] interval, and his `right` branch is empty
  * In other cases it contain left closed right open [) interval, and his `right` is non empty
  */
abstract class AbstractInternalNode[K, +V, U <: BoundType]
  extends NonEmptyITree[K, V, U]{

  val left: IntervalTree[K, V]
  val right: IntervalTree[K, V]

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

  override def lowerBound: Bound[K, Closed, Lower] = {
    left match {
      case empty: EmptyNode[K, V] => interval.lower
      case node @ InternalNode(_, _, _, _) => node.lowerBound
      case node @ UpperBoundInternalNode(_, _, _) => node.lowerBound
      case Leaf(i, _) => i.lower
      case UpperBoundLeaf(i, _) => i.lower
    }
  }

  override def upperBound: Bound[K, Closed, Upper] = {
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

final case class InternalNode[K, +V](
  override val interval: Interval[K, Closed, Open],
  override val v: V,
  override val left: IntervalTree[K, V],
  override val right: IntervalTree[K, V])
  extends AbstractInternalNode[K, V, Open] {

  def this(low: K, upp: K, v: V,
           left: IntervalTree[K, V],
           right: IntervalTree[K, V]) {
    this(Interval.unsafe(low, Closed, upp, Open), v, left, right)
  }

  def this(tuple: ((K, K), V),
           left: IntervalTree[K, V],
           right: IntervalTree[K, V]
  ) {
    this(tuple._1._1, tuple._1._2, tuple._2, left, right)
  }

  override def tuple: (Interval[K, Closed, Open], V) = (interval, v)

  override lazy val hasBoundNode: Boolean = right.hasBoundNode

  override def upp: K = interval.upper.bound


  override lazy val size: Int = 1 + left.size + right.size

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int): Array[((K, K), V1)] = {
    left.updateArray(array, posOfLeft(left, pos))
    array.update(pos, (interval.values, v))
    right.updateArray(array, posOfRight(right, pos))
  }


  override
  def sliceLower(x: K)(
    implicit ord: Ordering[K]): IntervalTree[K, V] = {
    x match{
      case c if interval.contains(x) => {
        new InternalNode[K, V](
          interval.withLower(x), v, EmptyNode[K, V], right
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

  override
  def sliceUpper(x: K)(
    implicit ord: Ordering[K]): IntervalTree[K, V] = {
    x match {
      case c if interval.contains(c) => {
        val lower = interval.lower.bound
        buildUpperBound(lower, c, v, left)
      }
      case l if upperThan(x) => {
        left.sliceUpper(x)
      }
      case r if lowerThan(x) => {
        right.sliceUpper(x) match{
          case empty: EmptyNode[K, V] => {
            val (lower, upper) = interval.values
            buildUpperBound(lower, upper, v, left)
          }
          case nonEmpty => new InternalNode(interval, v, left, nonEmpty)
        }
     }
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match{
      case that: InternalNode[k, v] => {
        that.canEqual(this) &&
          this.v == that.v &&
          this.interval == that.interval &&
          this.left == that.left &&
          this.right == that.right
      }
      case _ => false
    }
  }

  override def canEqual(that: Any): Boolean = {
    that match{
      case that: InternalNode[_, _] => true
      case _ => false
    }
  }

  override def hashCode(): Int = (v, interval, left, right).##

  override
  def contains(x: K)(implicit ord: Ordering[K]): Boolean = interval.contains(x)

  override
  def lowerThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isLowerThan(x)

  override
  def upperThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isUpperThan(x)


}

final case class UpperBoundInternalNode[K, V](
  override val interval: Interval[K, Closed, Closed],
  override val v: V,
  override val left: NonEmptyITree[K, V, Open])
  extends AbstractInternalNode[K, V, Closed] {

  def this(low: K, upp: K, v: V, left: NonEmptyITree[K, V, Open]){
    this(Interval.unsafe(low, Closed, upp, Closed), v, left)
  }

  def this(tuple: ((K, K), V), left: NonEmptyITree[K, V, Open]){
    this(tuple._1._1, tuple._1._2, tuple._2, left)
  }

  override
  def tuple: (Interval[K, Closed, Closed], V) = (interval, v)

  override val size: Int = 1 + left.size

  override lazy val hasBoundNode: Boolean = true

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int): Array[((K, K), V1)] = {
    left.updateArray(array, posOfLeft(left, pos))
    array.update(pos, (interval.values, v))
    array
  }

  override val right: IntervalTree[K, V] = EmptyNode[K, V]()

  override
  def sliceLower(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    x match{
      case c if interval.contains(x) => {
        new UpperBoundLeaf[K, V](x, upp, v)
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
              low, upp, v)
          }
        }
      }
      case r if this.lowerThan(x) => {
        this
      }
    }
  }

  override
  def sliceUpper(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    x match {
      case c if interval.contains(c) => {
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

  override def upperBound: Bound[K, Closed, Upper] = interval.upper


  override def equals(obj: Any): Boolean = {
    obj match{
      case that: UpperBoundInternalNode[k, v] => {
        that.canEqual(this) &&
          this.v == that.v &&
          this.interval == that.interval &&
          this.left == that.left
      }
      case _ => false
    }
  }

  override def canEqual(that: Any): Boolean = {
    that match{
      case that: UpperBoundInternalNode[_, _] => true
      case _ => false
    }
  }

  override def hashCode(): Int = (v, interval, left).##

  override
  def contains(x: K)(implicit ord: Ordering[K]): Boolean = interval.contains(x)

  override
  def lowerThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isLowerThan(x)

  override
  def upperThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isUpperThan(x)

}
