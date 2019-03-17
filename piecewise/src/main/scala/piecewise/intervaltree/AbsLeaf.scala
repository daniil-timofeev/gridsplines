package piecewise.intervaltree
abstract class AbsLeaf[+K, +V] extends NonEmptyInt[K, V] {
  val size = 1
}

case class Leaf[+K, +V](interval: Interval[K, Closed, Open], value: V)
  extends AbsLeaf[K, V]{
  type U = Open
  def this(low: K, upp: K, value: V){
    this(CsdOpn.pure.unsafe(low, upp), value)
  }
}
object Leaf {
  def apply[K, V](v: ((K, K), V)): Leaf[K, V] =
    new Leaf(v._1._1, v._1._2, v._2)
}
case class UpBoundLeaf[+K, +V](interval: Interval[K, Closed, Closed], value: V)
  extends AbsLeaf[K, V]{
  type U = Closed
  def this(low: K, upp: K, value: V){
    this(Csd.pure.unsafe(low, upp), value)
  }
}
object UpBoundLeaf {
  def apply[K, V](v: ((K, K), V)): UpBoundLeaf[K, V] =
    new UpBoundLeaf[K, V](v._1._1, v._1._2, v._2)
}



