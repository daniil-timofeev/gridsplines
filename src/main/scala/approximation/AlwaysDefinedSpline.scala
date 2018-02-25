package approximation

import piecewise.{PieceFunction, _}

class AlwaysDefinedSpline[P <: PieceFunction](spl: Spline[P]) {

  private val lowerX = spl.lowerBound
  private val upperX = spl.upperBound
  private val lowerY = spl(lowerX)
  private val upperY = spl(upperX)

  def apply(x: Double): Double = {
    if (x <= lowerX) lowerY
    else if (x >= upperX) upperY
    else spl(x)
  }

  def der(x: Double): Double = {
    if (x < lowerX || x > upperX) 0.0 else spl.der(x)
  }

  def area(xLow: Double, xUpp: Double): Double = {

    val lowerArea =
      if (xLow <= lowerX)
        if (xUpp <= lowerX) (xUpp - xLow) * lowerY
        else (lowerX - xLow) * lowerY
      else 0.0

    val upperArea =
      if (xUpp >= upperX)
        if (xLow >= upperX) (xUpp - xLow) * upperY
        else (xUpp - upperX) * upperY
      else 0.0

    val middleArea = {
      val low = math.max(xLow, lowerX)
      val upp = math.min(xUpp, upperX)
      spl.area(low, upp)
    }

    lowerArea + middleArea + upperArea
  }

  def average(xLow: Double, xUpp: Double): Double = {
    if (xLow == xUpp) apply(xLow)
    else area(xLow, xUpp) / (xUpp - xLow)
  }

}
