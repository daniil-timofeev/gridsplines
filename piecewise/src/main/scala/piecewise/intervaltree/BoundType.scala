package piecewise.intervaltree

sealed trait BoundType {

  def eq[I](bound: I, v: I)(ord: Ordering[I]): Boolean

  def isLowerThan[I, W <: Where](bound: I, v: I)(implicit B: W, ord: Ordering[I]): Boolean = {
    !eq(bound, v)(ord) && B.lowerThan(bound, v)
  }

  def isUpperThan[I, W <: Where](bound: I, v: I)(implicit B: W, ord: Ordering[I]): Boolean = {
    !eq(bound, v)(ord) && B.upperThan(bound, v)
  }

}
class Closed extends BoundType {
  override
  def eq[I](bound: I, v: I)(ord: Ordering[I]): Boolean = {
    ord.equiv(bound, v)
  }
}
class Open extends BoundType {
  def eq[I](bound: I, v: I)(ord: Ordering[I]): Boolean = {
    false
  }
}