package approximation

/**
  * Created by Даниил on 14.03.2017.
  */
sealed trait TypeDir{
  def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double): Array[Array[Double]]
}

class Ortho extends TypeDir{
  override def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double) = {
    arraygrid.makeOrthogonalMatrix(leftX, rangeX, rightX, sigma)
  }
}


class Radial extends TypeDir{
  override def preDef(leftX: Double, rangeX: Array[Double], rightX: Double, sigma: Double) = {
    arraygrid.makeRadialMatrix(leftX, rangeX, rightX, sigma)
  }
}

class Angular extends TypeDir{
  ??? //TODO implement angular direction based with use of arraygrid.makeOrthogonalMatrix method

  override def preDef(leftX: Double,
                      rangeX: Array[Double],
                      rightX: Double,
                      sigma: Double): Array[Array[Double]] = ???
}

