
package piecewise

package object intervaltree {

  implicit val Open = new Open
  implicit val Closed = new Closed

  implicit val Lower = new Lower
  implicit val Upper = new Upper


}
