package piecewise.intervaltree
abstract class AbsInNode[+K, +V] extends NonEmptyInt[K, V]{
  def left: AbsITree[K, V]
  def right: AbsITree[K, V]
  lazy val size = 1 + left.size + right.size

}

case class InNode[+K, +V](interval: Interval[K, Closed, Open],
                          value: V,
                          left: AbsITree[K, V],
                          right: AbsITree[K, V])
  extends AbsInNode[K, V] {
  type U = Open
  def this(low: K, upp: K, v: V, l: AbsITree[K, V], r: AbsITree[K, V]){
    this(CsdOpn.pure.unsafe(low, upp), v, l, r)
  }

}
object InNode {
  def apply[K, V](v: ((K, K), V),
                  l: AbsITree[K, V], r: AbsITree[K, V]): InNode[K, V] = {
    new InNode(v._1._1, v._1._2, v._2, l, r)
  }

}

case class UpBoundInNode[+K, +V](interval: Interval[K, Closed, Closed],
                                value: V,
                                left: AbsITree[K, V])
  extends AbsInNode[K, V]{
  type U = Closed
  def this(low: K, upp: K, v: V, l: AbsITree[K, V]){
    this(Csd.pure.unsafe(low, upp), v, l)
  }
  val right = EmptyInt
}
object UpBoundInNode {
  def apply[K, V](v: ((K, K), V), l: AbsITree[K, V]): UpBoundInNode[K, V] = {
    new UpBoundInNode(v._1._1, v._1._2, v._2, l)
  }

}
