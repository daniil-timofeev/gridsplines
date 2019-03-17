package piecewise.intervaltree
import cats._

import scala.languageFeature.higherKinds._
object BoundType
sealed trait BoundType {
  def isClosed: Boolean
  def eq[I](bound: I, v: I)(implicit ord: Order[I]): Boolean
}
class Closed extends BoundType {
  override def isClosed: Boolean = true
  def eq[I](bound: I, v: I)(implicit ord: Order[I]) = ord.eqv(bound, v)
}
class Open extends BoundType {
  override def isClosed: Boolean = false
  def eq[I](bound: I, v: I)(implicit ord: Order[I]) = false
}



