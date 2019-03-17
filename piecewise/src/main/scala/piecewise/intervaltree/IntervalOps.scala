package piecewise.intervaltree

import cats.kernel.Order

trait IntervalOps[I[_]]{

  def contain[V](interval: I[V], v: V)(implicit ord: Order[V]): Boolean
  def relation[V](interval: I[V], v: V)(implicit ord: Order[V]): Int

  def isLowerBound[V](interval: I[V], v: V)(implicit ord: Order[V]): Boolean
  def isUpperBound[V](interval: I[V], v: V)(implicit ord: Order[V]): Boolean


  def isUpperThan[V](interval: I[V], v: V)(implicit ord: Order[V]): Boolean =
    relation(interval, v) match {
      case GT => true
      case _ => false
    }

  def isLowerThan[V](interval: I[V], v: V)(implicit ord: Order[V]): Boolean =
    relation(interval, v) match {
      case LT => true
      case _ => false
    }

  def isEmptyWith[V](l: V, u: V)(implicit ord: Order[V]): Boolean = {
    ord.lt(l, u) || (ord.eqv(l, u) && haveClosedBound)
  }
  def haveClosedBound: Boolean

  def inside[V](interval: I[V])(l: V, u: V)(implicit ord: Order[V]): Boolean =

    isUpperThan[V](interval, l) && isLowerThan[V](interval, u)

  def outside[V](interval: I[V])(l: V, u: V)(implicit ord: Order[V]): Boolean =
   !inside(interval)(l, u)

}
