package piecewise

/**
  * Created by Даниил on 05.07.2017.
  */
object NoFunction extends PieceFunction{

  /** Значение функции в точке {@code x}
    * v of function at {@code x} point
    *
    * @param x точка, в которой ищется значение /
    *          point, where is yL of function searched */
  override def apply(x: Double): Double = ???

  /** Значение производной функции в точке {@code x}
    * v of derivative of function at {@code x} point
    *
    * @param x точка, в которой ищется значение производной /
    *          point, where is yL of function derivative searched */
  override def derivative(x: Double): Double = ???

  /** Значение интеграла функции в точке {@code x}
    * v of integral of function at {@code x} point
    *
    * @param x точка, в которой ищется значение интеграла функции /
    *          point, where is yL of function integral searched */
  override def integral(x: Double): Double = ???

  /** Экстремум функции `x`
    * Extremum of function `x`
    *
    * @return экстремумы функции / extremums of function */
  override protected def extremum: List[Double] = ???

  /** Приблизительное значение площади под функцией на интервале ``[x0:x1]``
    *
    * @param x0 нижняя граница
    * @param x1 верхняя граница
    * @return площадь функции на интерале ``[x0:x1]``
    */
  override def roughArea(x0: Double, x1: Double) = ???
}
