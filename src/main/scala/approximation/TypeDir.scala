package approximation

/**
  * Created by Даниил on 14.03.2017.
  */
sealed trait TypeDir{
  def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double): Array[Array[Double]]

  def toastHeatFlow(heatFlow: Double, x0: Double, x1: Double, x3: Double): Double

  def generalCoefs(lowX: Double, midX: Double, uppX: Double): Array[Double]

  def heatFlowCoefs(lowX: Double, uppX: Double): Double

  def analyticalCoefs(lowX: Double, uppX: Double): Double

}

class Ortho extends TypeDir{
  override def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double) = {
    arraygrid.makeOrthogonalMatrix(leftX, rangeX, rightX, sigma)
  }

  override def toastHeatFlow(heatFlow: Double, x0: Double, x1: Double, x3: Double) = ???

  override def generalCoefs(lowX: Double, midX: Double, uppX: Double): Array[Double] = {
    arraygrid.generalOrthoCoefs(lowX, midX, uppX)
  }

  override def heatFlowCoefs(lowX: Double, uppX: Double): Double = {
    1.0 / (uppX - lowX)
  }

  override def analyticalCoefs(lowX: Double, uppX: Double): Double = Double.NaN
}


class Radial extends TypeDir{
  override def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double) = {
    arraygrid.makeRadialMatrix(leftX, rangeX, rightX, sigma)
  }

  override def toastHeatFlow(heatFlow: Double,
                             r0: Double, r1: Double, r2: Double): Double = {
    val volume = arraygrid.vol(r0, r1, r2)
    heatFlow / (math.Pi * 2.0)
  }

  override def generalCoefs(lowX: Double, midX: Double, uppX: Double): Array[Double] =
    arraygrid.generalRadialCoefs(lowX, midX, uppX)

  override def heatFlowCoefs(lowX: Double, uppX: Double): Double = {
    val rAtHalf = (lowX + uppX) / 2.0
     rAtHalf  / (uppX - lowX) * math.Pi * 2.0
  }

  override def analyticalCoefs(lowX: Double, uppX: Double): Double = {
    math.log(uppX / lowX) / (math.Pi * 2)
  }
}

class Angular extends TypeDir{
  ??? //TODO implement angular direction based with use of arraygrid.makeOrthogonalMatrix method

  override def preDef(leftX: Double,
                      rangeX: Array[Double],
                      rightX: Double,
                      sigma: Double): Array[Array[Double]] = ???

  override def toastHeatFlow(heatFlow: Double, x0: Double, x1: Double, x3: Double) = ???

  override def generalCoefs(lowX: Double, midX: Double, uppX: Double) = ???

  override def heatFlowCoefs(lowX: Double, uppX: Double): Double = {
    ???
  }

  override def analyticalCoefs(lowX: Double, uppX: Double) = ???
}

