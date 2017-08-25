package approximation
import approximation.TwoDGrid._
import org.specs2._
import org.specs2.matcher._
import piecewise.Spline

class Coefficients extends Specification{def is = s2"""
               ${test}
               ${test0}
  """


  def test = {
    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 12.0)
    val bounds = Bounds(
      upp = new OneElementBound,
      low = new OneElementBound,
      left = new OneElementBound,
      right = new OneElementBound
    )

    val coef0 = VarXCoef(xD, (x0: Double, x1: Double) => Spline.const(1E-6))
    val coef1 = PatchXCoef(
      yD, (x0: Double, x1: Double) => Spline.const(2E-6),
      -1, 3, -1, 8
    )

    val coef = new PatchedXCoef(coef0, coef1)

    val grid = new TwoDGrid[Radial, Ortho](xD, yD, bounds, coef)

    val rC = grid.rowCoefs(5).toSet
    rC.size must_== 2
  }

  def test0 = {
    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 12.0)
    val bounds = Bounds(
      upp = new OneElementBound,
      low = new OneElementBound,
      left = new OneElementBound,
      right = new OneElementBound
    )

    val coef0 = VarYCoef(yD, (y0: Double, y1: Double) => Spline.const(1E-6))
    val coef1 = PatchYCoef(
      xD, (x0: Double, x1: Double) => Spline.const(2E-6),
      -1, 3, -1, 10
    )

    val coef = new PatchedYCoef(coef0, coef1)

    val grid = new TwoDGrid[Radial, Ortho](xD, yD, bounds, coef)

    val rC = grid.rowCoefs(5).toSet
    rC.size must_== 2
  }
}
