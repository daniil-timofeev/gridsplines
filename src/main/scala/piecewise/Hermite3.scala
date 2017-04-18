package piecewise

import com.twitter.algebird.{ ExclusiveUpper, InclusiveLower, Intersection }

import scala.math.{ pow, sqrt }

/**
 * Монотонная кусочная кубическая кривая для интерполяции / Monotonic piecewise cubic curve for interpolation
 * Кривая, предназначеная для аппроксимации явлений физической реальности, где требуется монотонность
 * / Сurve, that serve for approximation physical reality definitions, where is monotonic property required
 *
 * @see Fritsch, F. N. Monotone piecewise cubic interpolation / F. N. Fritsch, R. E. Carlson // SIAM J. Numer. Anal. — 1980. — 17. № 2. — pp. 238 — 246.
 * @version 0.5.0
 * @author Даниил
 */
case class Hermite3(protected val yL: Double, protected val yUp: Double,
                    protected val dL: Double, protected val dUp: Double,
                    override protected val low: Double, override protected val upp: Double)
  extends Hermite(low, upp) with Poly3 {

  /**
   * Трансформирует функцию так, чтобы она была монотонной /
   * Transform function to make it monotone
   *
   * @param fi гладкость спайна: {{{"Smooth", "Normal", "Coarse", "Coarsest"}}}
   *           Если во всех функциях в файле требуется одинаковая гладкость, можно импортировать неявное значение
   *           из объекта CubicHermitM1 /
   *           spline smoothnes:  {{{"Smooth", "Normal", "Coarse", "Coarsest"}}}
   *           If one smoothness required for all created splines, you may import implicit val from CubicHermitM1 object
   * @return монотонная функция / monotonic function
   */
  def monotone(implicit fi: String): M1Hermite3 = {

    def lessOf(a: Double, b: Double): Double = {
      if (a < 0 && a < b) b
      else if (a < 0 && b < a) a
      else if (b < 0 && b < a) a
      else if (b < 0 && a < b) b
      else math.min(a, b)
    }

    if (alpha.isNaN || beta.isNaN || alpha.isInfinity || beta.isInfinite) M1Hermite3(yL, yUp, 0.0, 0.0, low, upp)
    else {
      fi match {
        case "Smooth" => {
          M1Hermite3(yL, yUp, lessOf(dL, 3.0 * delta), lessOf(dUp, 3.0 * delta), low, upp)
        }
        case "Normal" => {
          val tau = 3 / sqrt(pow(alpha, 2.0) + pow(beta, 2.0))
          M1Hermite3(yL, yUp, lessOf(dL, tau * alpha * delta), lessOf(dUp, tau * beta * delta), low, upp)
        }
        case "Coarse" => {
          M1Hermite3(yL, yUp,
            lessOf(dL, 3.0 / (1.0 + beta / alpha) * delta),
            lessOf(dUp, 3.0 / (1.0 + alpha / beta) * delta),
            low, upp)
        }
        case "Coarsest" => {
          val c = if (alpha > beta) (1.0, 2.0) else (2.0, 1.0)
          M1Hermite3(yL, yUp,
            lessOf(dL, 3.0 / (c._2 + c._1 * beta / alpha) * delta),
            lessOf(dUp, 3.0 / (c._1 + c._2 * alpha / beta) * delta),
            low, upp)
        }
      }
    }
  }
}
object Hermite3 {

  def exception() = {
    throw new IllegalArgumentException(" размер values должен быть больше или равен двум /" +
      "/ the size of values must equal or more than two")
  }

  def apply(values: List[(Double, Double)]): Vector[Hermite3] = {


    values.sortBy(_ _1) match {
      case Nil => exception()
      case any :: Nil => exception()
      case v1 :: v2 :: Nil => Vector(new Hermite3(v1._2, v2._2, 0.0, 0.0, v1._1, v2._1))
      case v1 :: v2 :: v3 :: Nil => {
        val der1 = der(v1, v3)
        Vector(new Hermite3(v1._2, v2._2, 0.0, der1, v1._1, v2._1),
               new Hermite3(v1._2, v2._2, der1, 0.0, v2._1, v3._1))
      }
      case vals => {
        val v = vals.toVector
        val dervs = derivatives(v)
        ((v zip (v drop (1))) zip (dervs zip (dervs drop (1)))).map(tuple => {
          val (((x1, y1), (x2, y2)), (d1, d2)) = tuple
          new Hermite3(y1, y2, d1, d2, x1, x2)
        })
      }
    }
  }

  def apply(x: List[Double], y: List[Double]): Vector[Hermite3] = {
    if (x.length != y.length) throw new IllegalArgumentException("x array length must be same as y array length")
    //TODO Rewrite method to avoid data tupling
    apply(x.zip(y))
  }

  private def derivatives(values: Vector[(Double, Double)]): Vector[Double] = {
    val onBound = boundDervs(values)
    List.newBuilder[Double]
    val b = Vector.newBuilder[Double]
    b += onBound._1
    for (i <- 2 until values.size) {
      b += der(values(i - 2), values(i))
    }
    b += onBound._2
    b.result()
  }

  private def boundDervs(values: Vector[(Double, Double)]) = {
    val rightVals = values takeRight 2
    val der1 = der(values.head, values.tail.head)
    val der2 = der(rightVals.head, rightVals.tail.head)
    (der1, der2)
  }

  private def der(xy1: (Double, Double), xy2: (Double, Double)) = (xy2._2 - xy1._2) / (xy2._1 - xy1._1)

}