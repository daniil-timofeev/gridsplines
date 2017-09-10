package piecewise

/**
  * Created by Даниил on 16.03.2017.
  */
case class Const(value: Double) extends PieceFunction{

  override def apply(x: Double): Double = value

  override def derivative(x: Double): Double = 0.0

  override def integral(x: Double): Double = x * value

  /** Экстремум функции `x`
    * Extremum of function `x`
    *
    * @return экстремумы функции / extremums of function */
  override protected def extremum: List[Double] = value :: Nil

  override def equals(obj: scala.Any): Boolean = {
    obj match{
      case const: Const => value.equals(const.value)
      case _ => false
    }
  }

  /** Приблизительное значение площади под функцией на интервале ``[x0:x1]``
    *
    * @param x0 нижняя граница
    * @param x1 верхняя граница
    * @return площадь функции на интерале ``[x0:x1]``
    */
  override def roughArea(x0: Double, x1: Double) = (x1 - x0) * value
}
