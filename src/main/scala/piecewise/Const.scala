package piecewise
/**
  *
  */
case class Const(value: Double) extends PieceFunction{

  override def apply(x: Double): Double = value

  override def derivative(x: Double): Double = 0.0

  override def integral(x: Double): Double = x * value

  /** Extremum of the function of `x`
    * */
  override protected def extremum(low: Double, upp: Double): List[Double] = value :: Nil

  override def equals(obj: scala.Any): Boolean = {
    obj match{
      case const: Const => value.equals(const.value)
      case _ => false
    }
  }

  /** Area value at the ``[x0:x1]`` interval
    *
    * @param x0 low bound
    * @param x1 upper bound
    * @return area at the ``[x0:x1]`` interval
    */
  override def area(x0: Double, x1: Double): Double = (x1 - x0) * value
}
