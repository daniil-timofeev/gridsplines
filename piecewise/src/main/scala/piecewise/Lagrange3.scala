package piecewise
import piecewise.utils._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math._

/** The linear cubic spline with defect 1
  *
  * ='''Definition'''=
  * ''
  * ''' Cubic spline with defect 1,''' which interpolate the function at ''[''a, w'']'' interval'' and written as
  *
  * g(x) := {g,,k,,(x) := a,,k,, + b,,k,,(x-x,,k,,) + c,,k,,(x - x,,k,,)^2^ + d,,k,, (x - x,,k,,)^3^
  * where x ∈ [x,,k-1,,,k,,x,,] }^n^,,k=1,,
  * ''and satisfy the following conditions:
  * a. g(x,,k,,) = f,,k,, (spline function values is matched to the primordial values at the spline nodes);
  * b. g(x)  С^2^''[''a, w'']'' (the double continuing derivability);
  * c. g"(a) = g"(w) = 0 (bound conditions)
  * ''
  *
  * @see '''Вержбицкий В.М.''' Основы численных методов: Учебник для вузов
  *      / В.М. Вержбицкий — 3-e изд., стер. — М.: Высш. шк., 2009. — 840 с.: ил.
  * @version 0.0
  * @author Timofeev D.V.
  *
  */
case class Lagrange3(protected val coefs: Array[Double],
                     private[piecewise] val low: Double,
                     private[piecewise] val upp: Double
                    ) extends PieceFunction {

  override def apply(x: Double): Double =
    PieceFunction.cubicRuleOfHorner(x - low, coefs(0), coefs(1), coefs(2), coefs(3))

  private val derD = 3.0 * coefs(3)
  private val derC = 2.0 * coefs(2)

  override def derivative(x: Double) =
    PieceFunction.quadraticRuleOfHorner(x, coefs(1), derC, derD)

  /**
    * v of integral of function at {@code x} point
    *
    * @param x point, where is yL of function integral searched */
  override def antider(x: Double): Double =
    PieceFunction.cubicHornerIntegral(2, coefs(0), coefs(1), coefs(2), coefs(3))

  override def extremum(low: Double, upp: Double) = ???

  override def area(x0: Double, x1: Double) = ???

}
object Lagrange3{

  def apply(values : List[(Double, Double)]): List[Lagrange3] = {
    var c = cCoefs(values)
    var d = dCoefs(values, c)
    var b = bCoefs(values, c, d)
    var a = aCoefs(values)
    var to = values.drop(1).map(_._1); var from = values.map(_._1)
    val result = ListBuffer.empty[Lagrange3]

    while(c.nonEmpty) {
      result += new Lagrange3(
        Array(d.head, c.head, b.head, a.head),
        min(from.head, to.head),
        max(from.head, to.head)
      )
      a = a.tail; b = b.tail; c = c.tail; d = d.tail; to = to.tail; from = from.tail
    }
    result.result()
  }


  @tailrec private def cCoefs(values: List[(Double, Double)],
                              result: ListBuffer[(Double, Double, Double)] =
                              ListBuffer.empty[(Double, Double, Double)],
                      vector: ListBuffer[Double] = ListBuffer.empty[Double])
  : List[Double] = {
    values match{
      case t1 :: t2 :: t3 :: tail if result.isEmpty => {
        cCoefs(
          values.tail,
          result += coefs(hk(t2._1, t1._1),
          hk(t3._1, t2._1)),
          vector += vect(t1, t2, t3)
        )
      }
      case t1 :: t2 :: Nil => {
        val r: List[(Double, Double, Double)] =
          {result += coefs(hk(t2._1, t1._1), 0.0)}.result()
        val v: List[Double] = {vector += vect(t1, t2, t2)}.result()
        ListPassion.solve(r, v).head
      }
      case t1 :: t2 :: t3 :: tail => {
        cCoefs(values.tail, result += coefs(hk(t2._1, t1._1), hk(t3._1, t2._1)),
          vector += vect(t1, t2, t3))
      }
    }
  }


  @tailrec private def dCoefs(values: List[(Double, Double)],
                              c: List[Double],
                       result: ListBuffer[Double] =
                              ListBuffer.empty[Double]): List[Double]
  = {
    if((values.size - 2) != c.size) throw new UnsupportedOperationException(
      "list of values size minus 2 must be more than c list size")
    values match {
      case v1 :: v2 :: tail if result.isEmpty => {
        dCoefs(values.tail, c, result += (c.head - 0.0) / 3 * hk(v2._1, v1._1))
      }
      case v1 :: v2 :: Nil => {
        (result += (0.0 - c.head) / 3 * hk(v2._1, v1._1)).toList
      }
      case v1 :: v2 :: v3 :: tail => {
        dCoefs(
          values.tail,
          c.tail,
          result += (c.head - c.tail.head) / 3 * hk(v2._1, v1._1))
      }
    }
  }

  @tailrec private def bCoefs(values: List[(Double, Double)],
                              c: List[Double], d: List[Double],
                      result: ListBuffer[Double] =
                              ListBuffer.empty[Double]): List[Double] = {
    if(values.tail.nonEmpty){
      val h = hk(values.tail.head._1, values.head._1)
      bCoefs(values.tail, c.tail, d.tail,
        result += f(values.head, values.tail.head) +
          c.head * pow(h, 2) - d.head * pow(h, 3))
    }
    else result.toList


  }
  private def aCoefs(values: List[(Double, Double)]): List[Double] = {
    values.drop(1).map(_ _2)
  }


  private def vect(t1: (Double, Double),
                   t2: (Double, Double),
                   t3: (Double, Double)): Double =
    3*f(t2, t3) - 3*f(t1, t2)

  private def f(x1: (Double, Double),
                x2: (Double, Double)): Double = if(x2._1 - x1._1 != 0.0){
    (x2._2 - x1._2) / math.abs(x2._1 - x1._1)}
  else 0.0

  private def hk(x2: Double, x1: Double) = x2 - x1

  private def coefs(h1: Double, h2: Double): (Double, Double, Double) =
    (h1, 2 * (h2 + h1), h2)
}
