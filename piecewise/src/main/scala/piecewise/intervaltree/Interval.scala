package piecewise.intervaltree

import cats.Show

import scala.language.higherKinds

abstract class AbstractInterval[I, LT <: BoundType, UT <: BoundType] {

  def contains(v: I)(implicit L: LT, U: UT, ord: Ordering[I]): Boolean

  def isUpperThan(v: I)(implicit L: LT, ord: Ordering[I]): Boolean

  def isLowerThan(v: I)(implicit U: UT, ord: Ordering[I]): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

}

case class Interval[I, LT <: BoundType, UT <: BoundType](
  lower: Bound[I, LT, Lower], upper: Bound[I, UT, Upper])
  extends AbstractInterval[I, LT, UT] {

  def contains(v: I)(implicit L: LT, U: UT, ord: Ordering[I]): Boolean = {
    lower.consist(v) && upper.consist(v)
  }

  def isUpperThan(v: I)(implicit L: LT, ord: Ordering[I]): Boolean = {
    lower.isUpperThan(v)
  }

  def isLowerThan(v: I)(implicit U: UT, ord: Ordering[I]): Boolean = {
    upper.isLowerThan(v)
  }

  def values: (I, I) = (lower.bound, upper.bound)

  def withLower(newBound: I): Interval[I, LT, UT]  = {
    new Interval[I, LT, UT](lower.withValue(newBound), upper)
  }

  def withUpper(newBound: I): Interval[I, LT, UT] = {
    new Interval(lower, upper.withValue(newBound))
  }

  def toLeftClosedRightOpen: Interval[I, Closed, Open]= {
    new Interval(lower.toClosed, upper.toOpen)
  }

  def toLeftOpenRightClosed: Interval[I, Open, Closed] = {
    new Interval(lower.toOpen, upper.toClosed)
  }

  def toClosed: Interval[I, Closed, Closed] = {
    new Interval(lower.toClosed, upper.toClosed)
  }

  def toOpen: Interval[I, Open, Open] = {
    new Interval(lower.toOpen, upper.toOpen)
  }

  def isEmpty: Boolean = false

}

object Interval {

  def apply[
        I,
        L <: BoundType,
        U <: BoundType](
        low: I, lowType: L,  upp: I, uppType: U)(
        implicit ord: Ordering[I]): AbstractInterval[I, L, U] = {

    if ((Lower.eq(low, upp) && Upper.eq(upp, low)) ||
      lowType.eq(low, upp)(ord) ||
      uppType.eq(upp, low)(ord)) {
      unsafe[I, L, U](low, lowType, upp, uppType)
    }
    else Empty[I, L, U]()
  }

  def unsafe[
        I,
        L <: BoundType,
        U <: BoundType](
        low: I, lowType: L, upp: I, uppType: U): Interval[I, L, U] = {
    val lowBound = new Bound[I, L, Lower](low)
    val uppBound = new Bound[I, U, Upper](upp)
    new Interval[I, L, U](lowBound, uppBound)
  }

  implicit def showInterval[I, L <: BoundType, U <: BoundType](
                       implicit SL: Show[Bound[I, L, Lower]],
                                SU: Show[Bound[I, U, Upper]]) =
    new Show[Interval[I, L, U]] {
      override def show(interval: Interval[I, L, U]): String = {
        s"${SL.show(interval.lower)},${SU.show(interval.upper)}"
      }
    }
}


case class Empty[I,
  L <: BoundType,
  U <: BoundType]() extends AbstractInterval[I, L, U] {

  override
  def contains(v: I)(implicit L: L, U: U, ord: Ordering[I]): Boolean = false

  def isUpperThan(v: I)(implicit L: L, ord: Ordering[I]): Boolean = false

  def isLowerThan(v: I)(implicit U: U, ord: Ordering[I]): Boolean = false

  override
  def isEmpty: Boolean = true

}

case class Universe[I,
  L <: BoundType,
  U <: BoundType]() extends AbstractInterval[I, L, U] {

  override
  def contains(v: I)(implicit L: L, U: U, ord: Ordering[I]): Boolean = true

  def isUpperThan(v: I)(implicit L: L, ord: Ordering[I]): Boolean = false

  def isLowerThan(v: I)(implicit U: U, ord: Ordering[I]): Boolean = false


  override
  def isEmpty: Boolean = false

}
