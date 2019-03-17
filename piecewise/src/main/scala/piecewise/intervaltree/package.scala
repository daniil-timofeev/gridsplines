
package piecewise


package object intervaltree {

  val GT = 1
  val LT = -1
  val EQ = 0

  implicit val Open = new Open
  implicit val Closed = new Closed


}
