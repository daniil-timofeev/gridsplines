package piecewise.intervaltree

import cats.Show
import cats.kernel.Order

import scala.language.higherKinds

trait Bound[+I, T <: BoundType] extends Any

@specialized(Double, Float)
case class LowerBound[+I, T <: BoundType](bound: I) extends AnyVal with Bound[I, T] {
  def toOpen: LowerBound[I, Open] = new LowerBound[I, Open](bound)
  def toClosed: LowerBound[I, Closed] = new LowerBound[I, Closed](bound)
  def withValue[I0 >: I](x: I0): LowerBound[I0, T] = new LowerBound[I0, T](x)
  def relation[I0 >: I](v: I0)(
    implicit ctn: Bound.Contain[T, ({type B[i] = LowerBound[i, T]})#B],
             ord: Order[I0]
  ): Int = ctn.relation(this, v)
  def isBound[I0 >: I](v: I0)(implicit ord: Order[I0]): Boolean =
    ord.eqv(bound, v)
}
object LowerBound {
  type LowCsd[i] = LowerBound[i, Closed]
  type LowOpn[i] = LowerBound[i, Open]

  implicit val lowCsdContain = new Bound.Contain[Closed, LowCsd] {
    override def relation[V](interval: LowerBound[V, Closed],
                             v: V)(implicit ord: Order[V]): Int =
      if (ord.lteqv(interval.bound, v)) EQ
      else GT

    override def isLowerBound[V](interval: LowCsd[V],
                                 v: V)(implicit ord: Order[V]): Boolean =
      ord.eqv(interval.bound, v)

    override def isUpperBound[V](interval: LowCsd[V],
                                 v: V)(implicit ord: Order[V]): Boolean = false
  }
  implicit val lowsOpnContain = new Bound.Contain[Open, LowOpn] {
    override
    def relation[V](interval: LowerBound[V, Open],
                             v: V)(implicit ord: Order[V]): Int =
      if (ord.lt(interval.bound, v)) EQ
      else GT

    override def isLowerBound[V](interval: LowOpn[V],
                                 v: V)(implicit ord: Order[V]): Boolean =
      ord.eqv(interval.bound, v)

    override def isUpperBound[V](interval: LowOpn[V],
                                 v: V)(implicit ord: Order[V]): Boolean = false
  }

  implicit val lowCsd = Bound.ops[Closed, LowCsd]
  implicit val lowOpn = Bound.ops[Open, LowOpn]

  implicit def showCL[V](implicit sh: Show[V]): Show[LowerBound[V, Closed]] =
    Show.show((t: LowerBound[V, Closed]) => s"[${sh.show(t.bound)}")

  implicit def showOL[V](implicit sh: Show[V]): Show[LowerBound[V, Open]] =
    Show.show((t: LowerBound[V, Open]) => s"(${sh.show(t.bound)}")
}

@specialized(Double, Float)
case class UpperBound[+I, T <: BoundType](bound: I) extends AnyVal with Bound[I, T]{
  def toOpen: UpperBound[I, Open] = new UpperBound[I, Open](bound)
  def toClosed: UpperBound[I, Closed] = new UpperBound[I, Closed](bound)
  def withValue[I0 >: I](x: I0): UpperBound[I0, T] = new UpperBound[I0, T](x)
  def relation[I0 >: I](v: I0)(
    implicit ctn: Bound.Contain[T, ({type B[i] = UpperBound[i, T]})#B],
    ord: Order[I0]
  ): Int = ctn.relation(this, v)
  def isBound[I0 >: I](v: I0)(implicit ord: Order[I0]): Boolean =
    ord.eqv(bound, v)
}
object UpperBound {
  type UppCsd[i] = UpperBound[i, Closed]
  type UppOpn[i] = UpperBound[i, Open]

  implicit val uppCsdContain = new Bound.Contain[Closed, UppCsd] {
    override def relation[V](interval: UpperBound[V, Closed],
                             v: V)(implicit ord: Order[V]): Int =
      if (ord.gteqv(interval.bound, v)) EQ
      else LT

    override def isLowerBound[V](interval: UpperBound[V, Closed],
                                 v: V)(implicit ord: Order[V]): Boolean = false

    override def isUpperBound[V](interval: UpperBound[V, Closed],
                                 v: V)(implicit ord: Order[V]): Boolean =
      ord.eqv(interval.bound, v)
  }
  implicit val uppOpnContain = new Bound.Contain[Open, UppOpn] {
    override
    def relation[V](interval: UpperBound[V, Open],
                    v: V)(implicit ord: Order[V]): Int =
      if (ord.gt(interval.bound, v)) EQ
      else LT

    override
    def isLowerBound[V](interval: UpperBound[V, Open],
                        v: V)(implicit ord: Order[V]): Boolean =
      false

    override
    def isUpperBound[V](interval: UpperBound[V, Open],
                                 v: V)(implicit ord: Order[V]): Boolean =
      ord.eqv(interval.bound, v)
  }

  implicit val lowCsd = Bound.ops[Closed, UppCsd]
  implicit val lowOpn = Bound.ops[Open, UppOpn]

  implicit def showOU[V](implicit sh: Show[V]): Show[UpperBound[V, Open]] =
    Show.show((t: UpperBound[V, Open]) => s"${sh.show(t.bound)})")

  implicit def showCU[V](implicit sh: Show[V]): Show[UpperBound[V, Closed]] =
    Show.show((t: UpperBound[V, Closed]) => s"${sh.show(t.bound)}]")

}

object Bound {

  trait Contain[T <: BoundType, B[i] <: Bound[i, T]]{
    def relation[V](interval: B[V], v: V)(implicit ord: Order[V]): Int
    def contain[V](interval: B[V], v: V)(implicit ord: Order[V]): Boolean =
      relation[V](interval, v) == EQ
    def isLowerBound[V](interval: B[V], v: V)(implicit ord: Order[V]): Boolean
    def isUpperBound[V](interval: B[V], v: V)(implicit ord: Order[V]): Boolean
  }


  def ops[T <: BoundType,
          R[i] <: Bound[i, T]](implicit t: T,
                                        c: Contain[T, R]): IntervalOps[R] =
    new IntervalOps[R] {
      override
      def contain[V](interval: R[V],
                     v: V)(implicit ord: Order[V]): Boolean =
        c.contain(interval, v)

      override
      def isUpperThan[V](interval: R[V],
                         v: V)(implicit ord: Order[V]): Boolean =
        c.relation(interval, v) == 1

      override
      def isLowerThan[V](interval: R[V],
                         v: V)(implicit ord: Order[V]): Boolean =
        c.relation(interval, v) == -1

      override
      def isLowerBound[V](interval: R[V],
                          v: V)(implicit ord: Order[V]): Boolean =
        c.isLowerBound(interval, v)

      override
      def isUpperBound[V](interval: R[V],
                          v: V)(implicit ord: Order[V]): Boolean =
        c.isUpperBound(interval, v)

      override def haveClosedBound: Boolean = t.isClosed

      override
      def relation[V](interval: R[V],
                      v: V)(implicit ord: Order[V]): Int =
      c.relation(interval, v)
    }

  def lOpen[I](v: I): LowerBound[I, Open] = new LowerBound[I, Open](v)
  def lClosed[I](v: I): LowerBound[I, Closed] = new LowerBound[I, Closed](v)
  def uOpen[I](v: I): UpperBound[I, Open] = new UpperBound[I, Open](v)
  def uClosed[I](v: I): UpperBound[I, Closed] = new UpperBound[I, Closed](v)

}
