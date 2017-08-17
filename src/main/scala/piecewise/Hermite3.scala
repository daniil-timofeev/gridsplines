package piecewise

import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

import scala.collection.SeqView
import scala.collection.mutable.ListBuffer
import scala.math.{abs, pow, signum, sqrt}

/**
 * Монотонная кусочная кубическая кривая для интерполяции / Monotonic piecewise cubic curve for interpolation
 * Кривая, предназначеная для аппроксимации явлений физической реальности, где требуется монотонность
 * / Сurve, that serve for approximation physical reality definitions, where is monotonic property required
 *
 * @see Fritsch, F. N. Monotone piecewise cubic interpolation / F. N. Fritsch, R. E. Carlson // SIAM J. Numer. Anal. — 1980. — 17. № 2. — pp. 238 — 246.
 * @version 0.5.0
 * @author Даниил
 */
case class Hermite3(coefs: Array[Double], x0: Double) extends Hermite{

  def this(yLow: Double,  yUpp: Double,
          dLow: Double, dUpp: Double,
          low: Double, upp: Double,
          h: Double, delta: Double) {
   this(Array(
     yLow, dLow,
     (-2.0 * dLow - dUpp + 3.0 * delta) / h,
     (dLow + dUpp - 2.0 * delta) / pow(h, 2.0)
   ), low)
  }

  override def apply(x: Double): Double = PieceFunction.cubicRuleOfHorner(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def derivative(x: Double): Double = PieceFunction.cubicHornerDerivative(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def integral(x: Double): Double = PieceFunction.cubicHornerIntegral(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  private lazy val body = f"*(x${-x0}%+.7f)"

  override lazy val toString = {
    f"${coefs(3)}%1.4f" + body + f"^3 ${coefs(2)}%+1.4f" + body + f"^2  ${coefs(1)}%+1.4f" +
      body + f" ${coefs(0)}%+1.4f"
  }

  def swap(low: Double, upp: Double): Hermite3 = {
    val dLow = coefs(1)
    val dUpp = der(upp)
    ???
  }

  def extremum: List[Double] = ???
}
object Hermite3 {

  def h(low: Double, upp: Double) = upp - low

  def delta(yLow: Double, yUpp: Double, low: Double, upp: Double) = {
    (yUpp - yLow) / h(low, upp)
  }

  abstract class SmoothType{

    final def monotonize(alpha: Double, beta: Double): (Double, Double) = {
      if(alpha.isNaN || beta.isNaN || alpha.isInfinity || beta.isInfinite) (0.0, 0.0)
      else withType(alpha, beta)
    }

    protected def withType(alpha: Double, beta: Double): (Double, Double)
  }

  implicit object Smooth extends SmoothType{
    protected def withType(alpha: Double, beta: Double): (Double, Double) = {
      (math.min(alpha, 3.0), math.min(beta, 3.0))
    }
  }

  implicit object Normal extends SmoothType{
    protected def withType(alpha: Double, beta: Double): (Double, Double) = {
      val tau = 3.0 / sqrt(pow(alpha, 2.0) + pow(beta, 2.0))
      (math.min(alpha, tau * alpha), math.min(beta, tau * beta))
    }
  }

  implicit object Coarse extends SmoothType{
    protected def withType(alpha: Double, beta: Double): (Double, Double) = {
      (math.min(alpha, 3.0 / (1.0 + beta / alpha)),
        math.min(beta, 3.0 / (1.0 + alpha / beta)))
    }
  }

  implicit object Coarsest extends SmoothType{
    protected def withType(alpha: Double, beta: Double): (Double, Double) = {
      val c = if (alpha > beta) (1.0, 2.0) else (2.0, 1.0)
      (math.min(alpha, 3.0 / (c._2 + c._1 * beta / alpha)),
        math.min(beta, 3.0 / (c._1 + c._2 * alpha / beta)))
    }
  }

  def isMonotone(alpha: Double, beta: Double, dLow: Double, dUpp: Double, fi: Double) = {
    if((signum(dLow) == signum(dUpp) || dLow == 0 || dUpp == 0)){
      if(2 * alpha + beta - 3 <= 0.0 || alpha + 2 * beta - 3.0 <= 0 || fi >= 0) true else false
    } else false
  }

  /**
    * Трансформирует функцию так, чтобы она была монотонной /
    * Transform function to make it monotone
    *
    *          `SmoothType` гладкость спайна: {{{"Smooth", "Normal", "Coarse", "Coarsest"}}}
    *           Если во всех функциях в файле требуется одинаковая гладкость, можно импортировать неявное значение
    *           из объекта CubicHermitM1 /
    *           spline smoothnes:  {{{"Smooth", "Normal", "Coarse", "Coarsest"}}}
    *           If one smoothness required for all created splines, you may import implicit val from CubicHermitM1 object
    * @return монотонная функция / monotonic function
    */

  def monothone[S <: SmoothType](sources: Array[Double])(
    implicit coarser: S): Array[Double] = {

      val Array(yLow, yUpp, dLow, dUpp, xLow, xUpp) = sources

      val h = xUpp - xLow

      val delta = (yUpp - yLow) / h

      val alpha  = abs(dLow / delta)
      val beta = abs(dUpp / delta)

      if({yLow :: yUpp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные " +
        " значения функции должны быть вещественными числами / initial values of function must be not NaN")

      if({dLow :: dUpp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные" +
        " значение производных должны быть вещественными числами / initial values of derivatives must be not NaN")

      if({xLow :: xUpp :: Nil} exists(_.isNaN)) throw new IllegalArgumentException(" Исходные" +
        " значения аргументнов должны быть вещественными числами / initial values of arguments must be not NaN")

      def fi(alpha: Double, beta: Double): Double = alpha - 1.0/3.0*pow(2.0*alpha + beta - 3.0,2.0)/(alpha + beta - 2.0)

      val (smoothAlpha, smoothBeta) = coarser.monotonize(alpha, beta)
      val sdLow = smoothAlpha * delta
      val sdUpp = smoothBeta * delta
      if(!isMonotone(smoothAlpha, smoothBeta, sdLow, sdUpp, fi(smoothAlpha, smoothBeta)))
        throw new RuntimeException("Function after monotonization must be monothone")

    Array(yLow, yUpp, sdLow, sdUpp, xLow, xUpp)
  }

  def extremum(xLow: Double, h: Double, alpha: Double, beta: Double): Double =
    xLow + h / 3.0 * (2 * alpha + beta - 3.0)/(alpha + beta - 2)

  def exception() = {
    throw new IllegalArgumentException(" размер values должен быть больше или равен двум /" +
      "/ the size of values must equal or more than two")
  }

  def array(xLow: Double, xUpp: Double,
            dLow: Double, dUpp: Double,
            yLow: Double, yUpp: Double): Array[Double] = {
    Array(xLow, xUpp, dLow, dUpp, yLow, yUpp)
  }

  def makeSources(values: List[(Double, Double)]): Iterator[Array[Double]] = {
    values match {
      case Nil => exception()
      case any :: Nil => exception()
      case v1 :: v2 :: Nil => {
        Iterator(array(v1._2, v2._2, 0.0, 0.0, v1._1, v2._1))
      }
      case v1 :: v2 :: v3 :: Nil => {
        val der1 = deriv(v1, v2)
        val der2 = deriv(v2, v3)
        val der = (der1 + der2) / 2.0
        Iterator(
          array(v1._2, v2._2, 0.0, der, v1._1, v2._1),
          array(v2._2, v3._2, der, 0.0, v2._1, v3._1)
        )
      }
      case vals => {
        val dervs = derivatives(vals)
        (vals.sliding(2) zip dervs.sliding(2)).map(lists => {
          val (((x1, y1) :: (x2, y2) :: Nil), Seq(d1, d2)) = lists
          array(y1, y2, d1, d2, x1, x2)
        })
      }
    }
  }

  def apply(values: List[(Double, Double)]): List[Hermite3] = {
    makeSources(values).map{src =>
      val d_a = delta(src(0), src(1), src(4), src(5))
      val h0 = h(src(4), src(5))
      new Hermite3(src(0), src(1), src(2), src(3), src(4), src(5), h0, d_a)
    }.toList
  }



  def apply(x: List[Double], y: List[Double]): List[Hermite3] = {
    if (x.length != y.length) throw new IllegalArgumentException("x array length must be same as y array length")
    //TODO Rewrite method to avoid data tupling
    apply(x.zip(y))
  }

  private def derivatives(values : List[(Double, Double)]): Iterator[Double] = {
    val onBound = boundDervs(values)
    onBound._1 :: (values, values drop 2).zipped.map(deriv) :::
      onBound._2 :: Nil
    Iterator(onBound._1) ++
    values.sliding(3).map(list => deriv(list(0), list(2))) ++
    Iterator(onBound._2)
  }

  private def boundDervs(values: List[(Double, Double)]) = {
    val rightVals = values takeRight 2
    val der1 = deriv(values.head, values.tail.head)
    val der2 = deriv(rightVals.head, rightVals.tail.head)
    (der1, der2)
  }

  private def deriv(xy1: (Double, Double), xy2: (Double, Double)) = (xy2._2 - xy1._2) / (xy2._1 - xy1._1)

}