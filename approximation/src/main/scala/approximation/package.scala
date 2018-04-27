import piecewise._

package object approximation {

  implicit val ortho = new Ortho
  implicit val radial = new Radial
  implicit val left = new Left
  implicit val right = new Right

  def applyAlwaysDefined[S <: PieceFunction](spl: Spline[S])(arg: Double): Double = {
    spl(arg)
  }
}
