package piecewise

import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

import scala.annotation.tailrec
import scala.math._

/** Кусочная кваратичная функция
  * Created by Даниил on 06.02.2016.
  */
case class Lagrange2(override protected val coefs: Array[Double])
  extends Lagrange with Slicer{

  type SliceType = Lagrange2

   def this(coef: (Double, Double, Double)) = {
  this(Array(coef _3, coef _2, coef _1))
  }

  def this(v1: (Double, Double), v2: (Double, Double), v3: (Double, Double)) = {
    this(Lagrange2.polynominals(v1, v2, v3))
  }

  override def apply(x: Double): Double = PieceFunction.quadraticRuleOfHorner(x, coefs(0), coefs(1), coefs(2))

  def integral(x: Double) : Double =
    PieceFunction.cubicRuleOfHorner(x, 0.0, coefs(0), coefs(1) / 2.0, coefs(2) / 3.0)

  override def derivative(x: Double) : Double = coefs(2) * 2.0 * x + coefs(1)

  override def roughArea(x0: Double, x1: Double) = ???
  /** Экстремум функции
    * extremum of function
    *
    * @return экстремумы функции / extremums of function */
  override protected def extremum: List[Double] = List(- coefs(1) / (2 * coefs(2)))

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


  def sliceUpper(upper: Double): SliceType = this

  def sliceLower(lower: Double): SliceType = this

}
object Lagrange2 {

  def apply(vals : List[(Double, Double)]): List[Lagrange2] = {
    if(vals.size != 3) throw new IllegalArgumentException("Должно быть 3 точки / Must be 3 points")
    (vals, vals drop 1, vals drop 2).zipped map{(v1, v2, v3) =>{
      val (a, b, c) = polynominals(v1, v2, v3)
      val interval = PieceFunction.makeInterval(v1._1, v3._1)
      new Lagrange2(Array(c, b, a))
    }}
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





