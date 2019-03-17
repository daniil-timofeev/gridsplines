package approximation
import approximation.TwoDGrid._
import org.specs2._
import piecewise._

class Coefficients extends Specification{def is = s2"""
  Patched coefficient ${test}
  Not patched coefficient ${test0}
  """


  def test = {

    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 12.0)

    val coef0 = VarXCoef(xD,
      (x0: Double, x1: Double) => Spline.const(1.0),
      (x0: Double, x1: Double) => Spline.const(1E6),
      (x0: Double, x1: Double) => Spline.const(1E-6)
    )

    val coef1 = PatchXCoef(xD,
      (x0: Double, x1: Double) => Spline.const(2.0),
      (x0: Double, x1: Double) => Spline.const(1E6),
      (x0: Double, x1: Double) => Spline.const(2E-6),
      -1, 3, -1, 8
    )

    val coef = new PatchedXCoef(coef0, coef1)
    val grid = TwoDGrid(xD, yD)(One, Temp)(One, Temp)(One, Temp)(One, Temp)(coef)
    val rC = grid.rowCoefs(5).toSet

    rC.size must_== 2
  }

  def test0 = {
    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 12.0)

    val coef0 = VarYCoef(yD,
      (y0: Double, y1: Double) => Spline.const(1.0),
      (y0: Double, y1: Double) => Spline.const(1E6),
      (y0: Double, y1: Double) => Spline.const(1E-6)
    )

    val grid = TwoDGrid(xD, yD)(One, Temp)(One, Temp)(One, Temp)(One, Temp)(coef0)

    val rC = grid.rowCoefs(5).toSet
    rC.size must_== 1
  }
}
