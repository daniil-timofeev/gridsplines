package piecewise.intervaltree


import scala.language.higherKinds

abstract class AbstractLeaf[K, +V, U <: BoundType]
  extends NonEmptyITree[K, V, U]{

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case leaf: Leaf[Any, Any] => {
        this.interval.equals(leaf.interval) && this.v.equals(leaf.v)
      }
      case _ => false
    }}

  override private[intervaltree]
  def updateArray[V1 >: V](
      array: Array[((K, K), V1)], pos: Int): Array[((K, K), V1)] = {
      array.update(pos, (interval.values, v))
      array
  }

  override def lowerBound: Bound[K, Closed, Lower] = interval.lower

}

final case class Leaf[K, +V](
    override val interval: Interval[K, Closed, Open],
    override val v: V
  ) extends AbstractLeaf[K, V, Open]{

  def this(low: K, upp: K, v: V){
    this(Interval.unsafe[K, Closed, Open](low, Closed, upp, Open), v)
  }

  def this(tuple: ((K, K), V)){
    this(tuple._1._1, tuple._1._2, tuple._2)
  }

  override lazy val hasBoundNode: Boolean = false

  override val size: Int = 1

  override def upperBound: Bound[K, Closed, Upper] =
    throw new RuntimeException(
      "This method should never be called," +
      " because the most upper bound has type" +
      " of UpperBoundLeaf[K, V] or UpperBoundInternalNode[K, V]"
    )


  override def sliceLower(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    val upp = interval.upper.bound
    if (interval.contains(x)) {
      new Leaf(x, upp, v)
    }
    else if (ord.equiv(upp, x)) EmptyNode[K, V]()
    else this
  }

  override def sliceUpper(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    val low = interval.lower.bound
    if (interval.contains(x)) {
      new UpperBoundLeaf(low, x, v)
    }
    else if (interval.isLowerThan(x)) this
    else EmptyNode[K, V]()
  }



  override def equals(obj: Any): Boolean = {
    obj match{
      case that: Leaf[k, v] => {
        that.canEqual(this) &&
          this.v == that.v &&
          this.interval == that.interval
      }
    }
  }

  override def canEqual(that: Any): Boolean = {
    that match {
      case that: Leaf[t, k] => true
      case _ => false
    }
  }

  override def hashCode(): Int = (v, interval).##

  override
  def contains(x: K)(implicit ord: Ordering[K]): Boolean = interval.contains(x)

  override
  def lowerThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isLowerThan(x)

  override
  def upperThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isUpperThan(x)

}
final case class UpperBoundLeaf[K, +V](
  override val interval: Interval[K, Closed, Closed],
  override val v: V
  ) extends AbstractLeaf[K, V, Closed] {

  def this(low: K, upp: K, v: V){
    this(Interval.unsafe[K, Closed, Closed](low, Closed, upp, Closed), v)
  }

  def this(tuple: ((K, K), V)){
    this(tuple._1._1, tuple._1._2, tuple._2)
  }

  override lazy val hasBoundNode: Boolean = true

  override val size: Int = 1


  override
  def sliceLower(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    if (interval.contains(x)) {
      new UpperBoundLeaf(x, upp, v)
    }
    else if (interval.isLowerThan(x)) EmptyNode[K, V]()
    else this
  }

  override
  def sliceUpper(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    if (interval.contains(x)) {
      new UpperBoundLeaf(low, x, v)
    }
    else if (interval.isUpperThan(x)) EmptyNode[K, V]()
    else this
  }


  override def upperBound: Bound[K, Closed, Upper] = this.interval.upper

  override def equals(obj: Any): Boolean = {
    obj match{
      case that: UpperBoundLeaf[k, v] => {
        that.canEqual(this) &&
          this.v == that.v &&
          this.interval == that.interval
      }
    }
  }

  override def canEqual(that: Any): Boolean = {
    that match {
      case that: UpperBoundLeaf[t, k] => true
      case _ => false
    }
  }

  override def hashCode(): Int = (v, interval).##

  override
  def contains(x: K)(implicit ord: Ordering[K]): Boolean = interval.contains(x)

  override
  def lowerThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isLowerThan(x)

  override
  def upperThan(x: K)(implicit ord: Ordering[K]): Boolean = interval.isUpperThan(x)

}