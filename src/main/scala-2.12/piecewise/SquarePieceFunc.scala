package piecewise

import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

import scala.annotation.tailrec
import scala.math._

/** Кусочная кваратичная функция
  * Created by Даниил on 06.02.2016.
  */
case class SquarePieceFunc(val a : Double, val b : Double, val c : Double,
                           override val interval: Intersection[InclusiveLower, ExclusiveUpper, Double] )
  extends PieceFunction(interval) {

   def this(coef: (Double, Double, Double), interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) = {
    this(coef _1, coef _2, coef _3, interval)
  }

  def this(v1 : Tuple2[Double, Double], v2 : Tuple2[Double, Double], v3 : Tuple2[Double, Double]) = {
    this(SquarePieceFunc.polynominals(v1, v2, v3), PieceFunction.makeInterval(v1._1, v3._1))
  }


  def value(x: Double) : Double =  a * pow(x, 2) + b * x + c

  def integral(x: Double) : Double = a / 3.0 * pow(x, 3) + b /
    2.0 * pow(x, 2) + c * x

  def derivative(x: Double) : Double = a * 2 * x + b


  /** Экстремум функции
    * extremum of function
    *
    * @return экстремумы функции / extremums of function */
  override protected def extremum: List[Double] = List(- b / (2 * a))

  private[this] def format(d: Double) = d match {
    case a if a > 0 => " + " + formatKey(a)
    case a if a < 0 => val b = abs(a); " - " + formatKey(b)
    case a => " + 0"
  }

  @tailrec private[this] def formatKey(value: Double, power: Int = 0): String = {
    if (value < 1) formatKey(value * 10, power - 1)
    else if (value > 100.0) formatKey(value / 10, power + 1)
    else f"$value%1.2f" + "·" + f"10^$power%1.0f"
  }

}
object SquarePieceFunc {

  def apply(vals : List[(Double, Double)]): Vector[SquarePieceFunc] = {
    if(vals.size != 3) throw new IllegalArgumentException("Должно быть 3 точки / Must be 3 points")
    (vals.view, vals drop 1, vals drop 2).zipped map{(v1, v2, v3) =>{
      val (a, b, c) = polynominals(v1, v2, v3)
      val interval = PieceFunction.makeInterval(v1._1, v3._1)
      new SquarePieceFunc(a, b, c, interval)
    }} toVector
  }

  private def polynominals(v1 : Tuple2[Double, Double],
                           v2 : Tuple2[Double, Double],
                           v3 : Tuple2[Double, Double]) : (Double, Double, Double) = {
    val (x0, y0) = v1
    val (x1, y1) = v2
    val (x2, y2) = v3

    @inline def upperFunc(tuple2 : (Double, Double)) = {
      val (a, b) = tuple2
      val aa = 1.0
      val bb = - (a + b)
      val cc = a * b
      List(aa, bb, cc)}

    val d =
      (y0 / ((x0 - x1) * (x0 - x2))) ::
      (y1 / ((x1 - x0) * (x1 - x2))) ::
      (y2 / ((x2 - x0) * (x2 - x1))) ::
      Nil

    val result = List((x1, x2), (x0, x2), (x0, x1)).map(upperFunc(_))

    val finish: Array[Double] = Array[Double](0.0, 0.0, 0.0)
    var k: Int = 0
    while (k < result.size) {
      {
        var s: Int = 0
        while (s < d.length) {
          {
            finish(s) = finish(s) + result(k)(s) * d(k)
          }
          ({
            s += 1; s - 1
          })
        }
      }
      ({
        k += 1; k - 1
      })
    }
    (finish(0), finish(1), finish(2))
}
}





