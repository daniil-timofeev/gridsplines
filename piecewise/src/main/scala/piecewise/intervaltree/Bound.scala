package piecewise.intervaltree

import cats.Show

import scala.language.higherKinds

case class Bound[I, T <: BoundType, W <: Where](bound: I) extends AnyVal {

  def consist(v: I)(implicit T: T, B: W, ord: Ordering[I]): Boolean = {
    T.eq(bound, v)(ord) || B.eq(bound, v)
  }

  def isLowerThan(v: I)(implicit T: T, B: W, ord: Ordering[I]): Boolean = {
    T.isLowerThan(bound, v)(B, ord)
  }

  def isUpperThan(v: I)(implicit T: T, B: W, ord: Ordering[I]): Boolean = {
    T.isUpperThan(bound, v)(B, ord)
  }

  def withValue(v: I): Bound[I, T, W] = {
    new Bound[I, T, W](v)
  }

  def toOpen(v: I): Bound[I, Open, W] = new Bound[I, Open, W](v)

  def toOpen: Bound[I, Open, W] = new Bound[I, Open, W](bound)

  def toClosed(v: I): Bound[I, Closed, W] = new Bound[I, Closed, W](v)

  def toClosed: Bound[I, Closed, W] = new Bound[I, Closed, W](bound)

}
object Bound {

  implicit def showBound[I, T <: BoundType, W <: Where](
    implicit t: T, w: W, i: Show[I]): Show[Bound[I, T, W]] =
    new Show[Bound[I, T, W]]{
      override def show(value: Bound[I, T, W]): String = {
        w match {
          case Lower => {
            t match {
              case Open => s"(${i.show(value.bound)}"
              case Closed => s"[${i.show(value.bound)}"
            }
          }
          case Upper => {
            t match {
              case Open => s"${i.show(value.bound)})"
              case Closed => s"${i.show(value.bound)}]"
            }
          }
        }
      }
    }

}
