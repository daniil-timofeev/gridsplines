package approximation
import approximation.arraygrid._

/**
  * Created by Даниил on 14.03.2017.
  */
sealed trait TypeDir{

  def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double): Array[Array[Double]]

  def toastHeatFlow(heatFlow: Double, x0: Double, x1: Double, x3: Double): Double

  def lowerHeatFlow(lowX: Double, midX: Double, uppX: Double): (Double, Double)

  def upperHeatFlow(lowX: Double, midX: Double, uppX: Double): (Double, Double)

  def heatFlowCoefs(lowX: Double, uppX: Double): Double

  def analyticalCoefs(lowX: Double, uppX: Double): Double

}

class Ortho extends TypeDir{

  override def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double) = {
    arraygrid.makeOrthogonalMatrix(leftX, rangeX, rightX, sigma)
  }

  override def toastHeatFlow(heatFlow: Double, x0: Double, x1: Double, x3: Double) = ???

  override def lowerHeatFlow(lowX: Double, midX: Double, uppX: Double): (Double, Double) = {

      val height = ah(lowX, midX, uppX)
      val c = 1.0 / dh(midX, uppX)

    (height, c)
  }

  override def upperHeatFlow(lowX: Double, midX: Double, uppX: Double): (Double, Double) = {
    val height = ah(lowX, midX, uppX)
    val a = 1.0 / dh(lowX, midX)

    (height, a)
  }

  override def heatFlowCoefs(lowX: Double, uppX: Double): Double = {
    1.0 / (uppX - lowX)
  }

  override def analyticalCoefs(lowX: Double, uppX: Double): Double = heatFlowCoefs(lowX, uppX)
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

  override def lowerHeatFlow(lowR: Double, midR: Double, uppR: Double): (Double, Double) = {

    val volume = vol(lowR, midR, uppR)
    val c =  rAtHalf(midR, uppR) / dh(midR, uppR)

    (volume, c)
  }

  override def upperHeatFlow(lowR: Double, midR: Double, uppR: Double): (Double, Double) = {

    val volume = vol(lowR, midR, uppR)
    val a =  rAtHalf(lowR, midR) / dh(lowR, midR)

    (volume, a)
  }


  override def heatFlowCoefs(lowX: Double, uppX: Double): Double = {
    val rAtHalf = (lowX + uppX) / 2.0
     rAtHalf  / (uppX - lowX) * math.Pi * 2.0
  }



  def heatFlowCoefs(lowX: Double,
                    midX: Double,
                    uppX: Double): (Double, Double) = {
    (heatFlowCoefs(lowX, midX), heatFlowCoefs(midX, uppX))
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

  override def lowerHeatFlow(lowX: Double, midX: Double, uppX: Double) : (Double, Double) = ???

  override def heatFlowCoefs(lowX: Double, uppX: Double): Double = {
    ???
  }

  override def analyticalCoefs(lowX: Double, uppX: Double) = ???

  override def upperHeatFlow(lowX: Double, midX: Double, uppX: Double): (Double, Double) = ???
}

