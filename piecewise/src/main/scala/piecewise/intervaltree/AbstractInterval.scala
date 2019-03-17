package piecewise.intervaltree

import algebra.ring._
import cats.implicits._
import cats.kernel.Order
import cats.{Group, Monoid, Show}
import piecewise.intervaltree.Interval.{Empty, PureInt}

import scala.annotation.tailrec
import scala.language.higherKinds

sealed abstract class AbsInterval[+I] {
  def isEmpty: Boolean
}

abstract class Interval[+I, LT <: BoundType, UT <: BoundType]
  extends AbsInterval[I] {

  def lower: LowerBound[I, LT]
  def upper: UpperBound[I, UT]


  def unapply: (I, I) = (lower.bound, upper.bound)

  def withLower[I0 >: I](newBound: I0): Interval[I0, LT, UT]
  def withUpper[I0 >: I](newBound: I0): Interval[I0, LT, UT]
  def toLeftClosedRightOpen: Interval[I, Closed, Open] =
    CsdOpn(lower.toClosed, upper.toOpen)
  def toLeftOpenRightClosed: Interval[I, Open, Closed] =
    OpnCsd(lower.toOpen, upper.toClosed)
  def toClosed[I0 >: I]: Csd[I0] = Csd(lower.toClosed, upper.toClosed)
  def toOpen[I0 >: I]: Opn[I0] = Opn(lower.toOpen, upper.toOpen)
  def isEmpty: Boolean = false
  def length[I0 >: I](implicit g: Group[I0]): I0 =
    g.remove(upper.bound, lower.bound)

  def abs[I0 >: I](implicit g: Group[I0]): I0 = this.length[I0]

}

/** Interval with open bounds (...)
  *
  * @param lower lower bound
  * @param upper upper bound
  * @tparam I bound value type
  */
@specialized(Double, Float)
case class Opn[+I](lower: LowerBound[I, Open],
                   upper: UpperBound[I, Open]
  ) extends Interval[I, Open, Open] {

  override
  def withLower[I0 >: I](newBound: I0): Opn[I0] =
    Opn(lower.withValue(newBound), upper)

  override
  def withUpper[I0 >: I](newBound: I0): Opn[I0] =
    Opn(lower, upper.withValue(newBound))
  }
object Opn {

    implicit val ops: IntervalOps[Opn] =
    Interval.ops[Open, Open, Opn]

    implicit def show[K: Show]: Show[Opn[K]] =
    Interval.showInterval[K, Open, Open].contramap(x => x)

    implicit val pure: PureInt[Opn] = new PureInt[Opn] {
      override def apply[I: Order](l: I, u: I): AbsInterval[I] =
      if (implicitly[Order[I]].lt(l, u)) unsafe(l, u)
      else Empty

      override def unsafe[I](l: I, u: I): Opn[I] =
      Opn[I](LowerBound[I, Open](l), UpperBound[I, Open](u))

      override def low[I](i: Opn[I]): I = i.lower.bound
      override def upp[I](i: Opn[I]): I = i.upper.bound

      override def id[I](i: Opn[I]): AbsInterval[I] = i
    }
}

/** Interval with closed bound [...]
  *
  * @param lower lower bound [...
  * @param upper upper bound ...]
  * @tparam I bound value type
  */
@specialized(Double, Float)
case class Csd[+I](lower: LowerBound[I, Closed],
                  upper: UpperBound[I, Closed]
  ) extends Interval[I, Closed, Closed]{

  override
  def withLower[I0 >: I](newBound: I0): Csd[I0] =
    Csd(lower.withValue(newBound), upper)

  override
  def withUpper[I0 >: I](newBound: I0): Csd[I0] =
    Csd(lower, upper.withValue(newBound))
}
object Csd {

  implicit val ops: IntervalOps[Csd] =
  Interval.ops[Closed, Closed, Csd]

  implicit def show[K: Show]: Show[Csd[K]] =
  Interval.showInterval[K, Closed, Closed].contramap(x => x)

  implicit val pure: PureInt[Csd] = new PureInt[Csd] {
    override def apply[I: Order](l: I, u: I): AbsInterval[I] =
    if (implicitly[Order[I]].lteqv(l, u)) unsafe(l, u)
    else Empty

    override def unsafe[I](l: I, u: I): Csd[I] =
    Csd[I](LowerBound[I, Closed](l), UpperBound[I, Closed](u))

    override def low[I](i: Csd[I]): I = i.lower.bound
    override def upp[I](i: Csd[I]): I = i.upper.bound

    override def id[I](i: Csd[I]): AbsInterval[I] = i
  }


}

/** Interval with Open / Closed bound (...]
  *
  * @param lower lower bound (...
  * @param upper upper bound ...]
  * @tparam I bound value type
  */
@specialized(Double, Float)
case class OpnCsd[+I](lower: LowerBound[I, Open],
                      upper: UpperBound[I, Closed]
  ) extends Interval[I, Open, Closed]{

  override
  def withLower[I0 >: I](newBound: I0): OpnCsd[I0] =
    OpnCsd(lower.withValue(newBound), upper)

  override
  def withUpper[I0 >: I](newBound: I0): OpnCsd[I0] =
    OpnCsd(lower, upper.withValue(newBound))

}
object OpnCsd {

  implicit val ops: IntervalOps[OpnCsd] =
  Interval.ops[Open, Closed, OpnCsd]

  implicit def show[K: Show]: Show[OpnCsd[K]] =
  Interval.showInterval[K, Open, Closed].contramap(x => x)

  implicit val pure: PureInt[OpnCsd] = new PureInt[OpnCsd] {
    override def apply[I: Order](l: I, u: I): AbsInterval[I] =
    if (implicitly[Order[I]].lteqv(l, u)) unsafe(l, u)
    else Empty

    override def unsafe[I](l: I, u: I): OpnCsd[I] =
    OpnCsd[I](LowerBound[I, Open](l), UpperBound[I, Closed](u))

    override def low[I](i: OpnCsd[I]): I = i.lower.bound
    override def upp[I](i: OpnCsd[I]): I = i.upper.bound

    override def id[I](i: OpnCsd[I]): AbsInterval[I] = i
  }


}

/** Interval with Closed / Open bounds [...)
  *
  * @param lower lower bound [...
  * @param upper upper bound ...)
  * @tparam I bound value type
  */
@specialized(Double, Float)
case class CsdOpn[+I](lower: LowerBound[I, Closed],
                      upper: UpperBound[I, Open])
  extends Interval[I, Closed, Open]{

  override
  def withLower[I0 >: I](newBound: I0): CsdOpn[I0] =
    CsdOpn(lower.withValue(newBound), upper)

  override
  def withUpper[I0 >: I](newBound: I0): CsdOpn[I0] =
    CsdOpn(lower, upper.withValue(newBound))
}
object CsdOpn {

  implicit val ops: IntervalOps[CsdOpn] =
  Interval.ops[Closed, Open, CsdOpn]

  implicit def show[K: Show]: Show[CsdOpn[K]] =
  Interval.showInterval[K, Closed, Open].contramap(x => x)

  implicit val pure: PureInt[CsdOpn] = new PureInt[CsdOpn] {
    override def apply[I: Order](l: I, u: I): AbsInterval[I] =
    if (implicitly[Order[I]].lteqv(l, u)) unsafe(l, u)
    else Empty

    override def unsafe[I](l: I, u: I): CsdOpn[I] =
    CsdOpn[I](LowerBound[I, Closed](l), UpperBound[I, Open](u))

    override def low[I](i: CsdOpn[I]): I = i.lower.bound
    override def upp[I](i: CsdOpn[I]): I = i.upper.bound

    override def id[I](i: CsdOpn[I]): AbsInterval[I] = i
  }
}

/** Empty interval */
class Empty extends AbsInterval[Nothing]{
  def isEmpty: Boolean = true
}

/** Universe interval */
class Universe extends AbsInterval[Nothing]{
  def isEmpty: Boolean = false
}

object Interval {

    case object Empty extends Empty
    case object Universe extends Universe


  trait PureInt[T[i] <: AbsInterval[i]] {self =>

    def low[I](i: T[I]): I
    def upp[I](i: T[I]): I
    def apply[I: Order](l: I, u: I): AbsInterval[I]
    def unsafe[I](l: I, u: I): T[I]
    def withLower[I: Order](l: I)(i: T[I]): AbsInterval[I] = apply(l, upp(i))
    def withUpper[I: Order](u: I)(i: T[I]): AbsInterval[I] = apply(low(i), u)
    def id[I](i: T[I]): AbsInterval[I]
    def abs[I](i: T[I])(implicit g: Group[I]): I = g.remove(upp(i), low(i))
    def sections[I](n: Int)(i: T[I])(implicit f: Field[I]): List[T[I]] = {
      if (i.isEmpty || n == 1) List(i)
      else {
        val additive = f.additive
        val v = f.fromInt(n)
        val absVal: I = self.abs(i)(additive)
        val add: I = f.div(absVal, v)
        val xs = Iterator.iterate(self.low(i))(l => additive.combine(l, add))
                       .take(n) ++ Iterator(self.upp(i))
        xs.sliding(2).map{case Seq(l: I, u: I) => self.unsafe(l, u)}.toList
      }
    }

    def sectionsWithMaxLength[I](l: I)(i: T[I])(
      implicit f: Field[I], ord: Order[I]): List[T[I]] = {
      if (i.isEmpty) List(i)
      else {
        val absVal: I = self.abs(i)(f.additive)
        val parts = roundUp(0)(f.div(absVal, l))
        if (parts == 0) sections(1)(i)
        else  sections(parts)(i)
      }
    }

    @tailrec
    private
    final def roundUp[I](n: Int)(i: I)(implicit f: Field[I],
                                                ord: Order[I]): Int = {
      val v = f.fromInt(n)
      if (ord.gteqv(v, i)) n
      else roundUp(n + 1)(i)
    }

  }


    def slice[T[i] <: AbsInterval[i], I](interval: T[I])(l: I, u: I)(
      implicit ops: IntervalOps[T],
               ord: Order[I],
               cons: PureInt[T]
    ): AbsInterval[I] = {
    if (ord.lt(u, l)) Empty
    else interval match {
      case i: Interval[I, _, _] =>
        cons.apply[I](ord.max(i.lower.bound, l), ord.min(i.upper.bound, u))
      case empty => empty
    }
  }

  def contains[T[_], I](interval: T[I])(v: I)(
    implicit ops: IntervalOps[T],
             ord: Order[I]): Boolean = ops.contain(interval, v)

  def inside[T[_], I](interval: T[I])(l: I, u: I)(
    implicit ops: IntervalOps[T],
             ord: Order[I]): Boolean = ops.inside(interval)(l, u)

  def isUpperThan[T[_], I](interval: T[I])(v: I)(
  implicit ops: IntervalOps[T],
  ord: Order[I]): Boolean = ops.isUpperThan(interval, v)

  def relateTo[K: Order,
               T[_]: IntervalOps](interval: T[K])(x: K)(
    implicit ops: IntervalOps[T]): Int = ops.relation(interval, x)

  def isLowerThan[T[_], I](interval: T[I])(v: I)(
    implicit ops: IntervalOps[T], ord: Order[I]): Boolean =
    ops.isLowerThan(interval, v)

    def ops[LT <: BoundType, UT <: BoundType, R[i] <: Interval[i, LT, UT]](
                      implicit lOps: IntervalOps[({type L[i] = LowerBound[i, LT]})#L],
                               uOps: IntervalOps[({type U[i] = UpperBound[i, UT]})#U]
     ): IntervalOps[R] = {
      new IntervalOps[R]{

        override
        def contain[I](interval: R[I], v: I)(
          implicit ord: Order[I]): Boolean = relation(interval, v) == EQ

        override
        def isLowerBound[I](interval: R[I], v: I)(
          implicit ord: Order[I]): Boolean =
          lOps.isLowerBound[I](interval.lower, v)

        override
        def isUpperBound[I](interval: R[I], v: I)(
          implicit ord: Order[I]): Boolean =
          uOps.isUpperBound[I](interval.upper, v)

        override
        def relation[V](interval: R[V], v: V)(
          implicit ord: Order[V]): Int =
          if (lOps.contain[V](interval.lower, v))
            if (uOps.contain[V](interval.upper, v)) EQ
            else LT
        else GT

        override
        def haveClosedBound: Boolean = uOps.haveClosedBound || lOps.haveClosedBound
      }
    }

    def show[I](i: AbsInterval[I])(
      implicit SL0: Show[LowerBound[I, Closed]],
               SL1: Show[LowerBound[I, Open]],
               SU0: Show[UpperBound[I, Closed]],
               SU1: Show[UpperBound[I, Open]]
    ): String =
      i match {
        case Empty => "()"
        case opn: Opn[I] => showInterval[I, Open, Open].show(opn)
        case csd: Csd[I] => showInterval[I, Closed, Closed].show(csd)
        case opnCsd: OpnCsd[I] => showInterval[I, Open, Closed].show(opnCsd)
        case csdOpn: CsdOpn[I] => showInterval[I, Closed, Open].show(csdOpn)
      }

    implicit def showInterval[I, L <: BoundType, U <: BoundType](
                          implicit SL: Show[LowerBound[I, L]],
                                   SU: Show[UpperBound[I, U]]
      ): Show[Interval[I, L, U]] =
      new Show[Interval[I, L, U]] {
        override def show(interval: Interval[I, L, U]): String = {
          s"${SL.show(interval.lower)},${SU.show(interval.upper)}"
        }
      }


    @specialized(Double, Float)
    def foldWithFilter[I, L <: BoundType, U <: BoundType, V, R]
    (low: I, upp: I)(in: Interval[I, L, U], v: V)(f: (I, I, V) => R)(
      implicit ord: Order[I], g: Group[I], m: Monoid[R]
              ): R = {
      val newLow = ord.max(low, in.lower.bound)
      val newUpp = ord.min(upp, in.upper.bound)
      val diff = g.remove(newUpp, newLow)
      if (ord.lteqv(diff, g.empty)) m.empty
      else f(newLow, newUpp, v)
    }

  def length[I](absInterval: AbsInterval[I])(implicit g: Group[I]): I =
    absInterval match {
      case Empty => g.empty
      case nonEmpty: Interval[I, _, _] =>
        g.remove(nonEmpty.upper.bound, nonEmpty.lower.bound)
    }

}

