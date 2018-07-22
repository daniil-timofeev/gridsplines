package piecewise.intervaltree

sealed abstract class Where {

  def eq[V: Ordering](bound: V, value: V): Boolean

  def lowerThan[V: Ordering](bound: V, value: V): Boolean

  def upperThan[V: Ordering](bound: V, value: V): Boolean
}

class Lower extends Where {

  override
  def eq[V: Ordering](bound: V, value: V): Boolean = {
    implicitly[Ordering[V]].lt(bound, value)
  }

  override
  def lowerThan[V: Ordering](bound: V, value: V): Boolean = eq(bound, value)

  override
  def upperThan[V: Ordering](bound: V, value: V): Boolean = !eq(bound, value)


}

class Upper extends Where {

  override
  def eq[V: Ordering](bound: V, value: V): Boolean = {
    implicitly[Ordering[V]].gt(bound, value)
  }

  override
  def lowerThan[V: Ordering](bound: V, value: V): Boolean = !eq(bound, value)

  override
  def upperThan[V: Ordering](bound: V, value: V): Boolean = eq(bound, value)


}