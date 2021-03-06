package piecewise

import cats.Show

import scala.collection.mutable.ListBuffer
import scala.math.{abs, pow, signum, sqrt}

/**
 * Non monotonic cubic Hermite curve
 *
 *
 * @see Fritsch, F. N. Monotone piecewise cubic interpolation
  *      / F. N. Fritsch, R. E. Carlson
  *      // SIAM J. Numer. Anal. — 1980. — 17. № 2. — pp. 238 — 246.
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

  override def apply(x: Double): Double =
    PieceFunction.cubicRuleOfHorner(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def derivative(x: Double): Double =
    PieceFunction.cubicHornerDerivative(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  override def antider(x: Double): Double =
    PieceFunction.cubicHornerIntegral(x - x0, coefs(0), coefs(1), coefs(2), coefs(3))

  private lazy val body = f"*(x${-x0}%+.7f)"

  override lazy val toString = {
    f"${coefs(3)}%1.4f" + body + f"^3 ${coefs(2)}%+1.4f" +
      body + f"^2  ${coefs(1)}%+1.4f" +
      body + f" ${coefs(0)}%+1.4f"
  }

  def swap(low: Double, upp: Double): Hermite3 = {
    val dLow = coefs(1)
    val dUpp = der(upp)
    ???
  }

  def extremum(low: Double, upp: Double): List[Double] = ???

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case sameType: Hermite3 => {
        coefs.zip(sameType.coefs).forall(t => t._1.equals(t._2)) &&
        x0.equals(sameType.x0)
      }
      case _ => false
    }
  }

  override def area(x0: Double, x1: Double) = ???
}
object Hermite3 {

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

  implicit val show: Show[Hermite3] = new Show[Hermite3] {
    override def show(t: Hermite3): String = t.toString
  }

  /** Check if function is monotone
    *
    * @param alpha
    * @param beta
    * @param dLow
    * @param dUpp
    * @param fi
    * @return
    */
  protected
  def isMonotone(alpha: Double,
                 beta: Double,
                 dLow: Double,
                 dUpp: Double,
                 fi: Double): Boolean = {
    (signum(dLow) == signum(dUpp) || dLow == 0 || dUpp == 0) &&
      (2 * alpha + beta - 3 <= 0.0 || alpha + 2 * beta - 3.0 <= 0 || fi >= 0)
  }

  /**
    *
    * Transform function to make it monotone
    *           spline smoothnes:  {{{"Smooth", "Normal", "Coarse", "Coarsest"}}}
    *           If one smoothness required for all created splines,
    *           you may import implicit val from CubicHermitM1 object
    * @return monotonic function
    */
  def monothone[S <: SmoothType](sources: Array[Double])(
    implicit coarser: S): Array[Double] = {

      val Array(yLow, yUpp, dLow, dUpp, xLow, xUpp) = sources

      val h = xUpp - xLow

      val delta = (yUpp - yLow) / h

      val alpha  = abs(dLow / delta)
      val beta = abs(dUpp / delta)

      if({yLow :: yUpp :: Nil} exists(_.isNaN))
        throw new IllegalArgumentException("initial values of function must be not NaN")

      if({dLow :: dUpp :: Nil} exists(_.isNaN))
        throw new IllegalArgumentException("initial values of derivatives must be not NaN")

      if({xLow :: xUpp :: Nil} exists(_.isNaN))
        throw new IllegalArgumentException("initial values of arguments must be not NaN")

      def fi(alpha: Double, beta: Double): Double =
        alpha - 1.0/3.0*pow(2.0*alpha + beta - 3.0,2.0)/(alpha + beta - 2.0)

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
    throw new IllegalArgumentException("The size of values must equal or more than two")
  }

  private[piecewise]
  def array(xLow: Double, xUpp: Double,
            dLow: Double, dUpp: Double,
            yLow: Double, yUpp: Double): Array[Double] = {
    Array(xLow, xUpp, dLow, dUpp, yLow, yUpp)
  }

  def makeSources(values: Iterator[(Double, Double)]): Iterator[Array[Double]] = {
    new Iterator[Array[Double]] {
      val vals = values
      var isFirst = true
      var isLast = true

      var v1: (Double, Double) = _
      var v2: (Double, Double) = _
      var der: Double = _

      var last: Array[Double] = _

      override def hasNext: Boolean = vals.hasNext || isLast



      override def next(): Array[Double] = {
        if (isFirst && hasNext){
          isFirst = false
          val v0 = vals.next()
          if (vals.hasNext) {
            v1 = vals.next()
            if (vals.hasNext) {
              v2 = vals.next()
              if (vals.hasNext) {
                val der1 = deriv(v0, v1)
                val der2 = deriv(v0, v2)
                der = der2
                isLast = false
                array(v0._2, v1._2, der1, der2, v0._1, v1._1)
              }
              else {
                isLast = false
                val der1 = deriv(v0, v1)
                val der2 = deriv(v1, v2)
                der = (der1 + der2) / 2.0
                last = array(v1._2, v2._2, der, 0.0, v1._1, v2._1)
                array(v0._2, v1._2, 0.0, der, v0._1, v1._1)
              }
            }
            else {
              isLast = false
              array(v0._2, v1._2, 0.0, 0.0, v0._1, v1._1)
            }
          }
          else throw new IllegalArgumentException(
            "Iterator must link to more than 1 value"
          )
        }
        else if (vals.hasNext){
          val v3 = vals.next()
          val der2 = deriv(v1, v3)
          val res = array(v1._2, v2._2, der, der2, v1._1, v2._1)
          v1 = v2
          v2 = v3
          der = der2
          if (vals.isEmpty) {
            val der2 = deriv(v1, v2)
            last = array(v1._2, v2._2, der, der2, v1._1, v2._1)
          }
          res
        }
        else {
          isLast = false
          last
        }
      }
    }
  }

  final def makeSources(values: List[(Double, Double)],
                        der1: Double,
                        acc: ListBuffer[Array[Double]] = ListBuffer.empty
                       ): Iterator[Array[Double]] = {
    values match {
      case Nil => Iterator.empty
      case v0 :: Nil => Iterator.empty
      case v0 :: v1 :: Nil => {
        acc += array(v0._2, v1._2, der1, deriv(v0, v1), v0._1, v1._1)
        acc.iterator
      }
      case v0 :: v1 :: v2 :: xs => {
        val der2 = deriv(v0, v2)
        makeSources(values.tail, der2, acc += array(v0._2, v1._2, der1, der2, v0._1, v1._1))
      }
    }
  }

  def apply(values: Iterator[(Double, Double)]): Iterator[Hermite3] = {
    val vals = values.toList
    makeSources(vals, deriv(vals.head, vals.tail.head)).map{src =>
      val d_a = delta(src(0), src(1), src(4), src(5))
      val h0 = h(src(4), src(5))
      new Hermite3(src(0), src(1), src(2), src(3), src(4), src(5), h0, d_a)
    }
  }

  def applyIncremental(
        values: Iterator[(Double, Double)]
        ): Iterator[((Double, Double), Hermite3)] = {
    makeSources(values).map{src =>
      val d_a = delta(src(0), src(1), src(4), src(5))
      val h0 = h(src(4), src(5))
      ((src(4), src(5)),
        new Hermite3(src(0), src(1), src(2), src(3), src(4), src(5), h0, d_a))
    }
  }

  def apply(x: List[Double], y: List[Double]): Iterator[Hermite3] = {
    if (x.length != y.length)
      throw new IllegalArgumentException("x array length must be same as y array length")
    apply(x.iterator.zip(y.iterator))
  }

}