package piecewise
import approximation.passion.{ListPassion, Passion}
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math._

/** Интерполяционный кубический сплайн дефекта 1 /
  * Linear cubic spline with defect 1
  *
  * ='''Определение'''=
  * ''
  * ''' Кубическим сплайном дефекта 1,''' интерполирующим на отрезке ''[''a, w'']'' данную
  * функцию f(x), называется функция
  * g(x) := {g,,k,,(x) := a,,k,, + b,,k,,(x-x,,k,,) + c,,k,,(x - x,,k,,)^2^ + d,,k,, (x - x,,k,,)^3^
  * при x принадлежащим [x,,k-1,,,k,,x,,] }^n^,,k=1,,
  * удовлетворяющая совокупности условий:
  * a. g(x,,k,,) = f,,k,, (условие интерполяции в узлах сплайна);
  * б. g(x) принадлежащая С^2^''[''a, w'']'' (двойная непрерывная дифференцируемость);
  * в. g"(a) = g"(w) = 0 (краевые условия)
  * ''
  *
  * @see '''Вержбицкий В.М.''' Основы численных методов: Учебник для вузов
  *      / В.М. Вержбицкий — 3-e изд., стер. — М.: Высш. шк., 2009. — 840 с.: ил.
  * @version 0.0
  * @author Тимофеев Д.В. / Timofeev D.V.
  *
  */
case class CLagrange(a: Double, b: Double, c: Double, d: Double,
                     override val interval: Intersection[InclusiveLower, ExclusiveUpper, Double])
  extends PieceFunction(interval){


  def apply(x: Double): Double = PieceFunction.cubicRuleOfGorner(x - l, d, c, b, a)

  private val derD = 3.0 * d
  private val derC = 2.0 * c

  override def derivative(x: Double) = PieceFunction.squaredRuleOfGorner(x, derD, derC, b)

  /** Значение интеграла функции в точке {@code x}
    * value of integral of function at {@code x} point
    *
    * @param x точка, в которой ищется значение интеграла функции / point, where is yL of function integral searched */
  override def integral(x: Double): Double = ???

  override def extremum = ???
}
object CLagrange{

  def apply(values : List[Tuple2[Double, Double]]): List[CLagrange] = {
    var c = cCoefs(values)
    var d = dCoefs(values, c)
    var b = bCoefs(values, c, d)
    var a = aCoefs(values)
    var to = values.drop(1).map(_._1); var from = values.map(_._1)
    val result : ListBuffer[CLagrange] = ListBuffer.empty[CLagrange]
    while(c.nonEmpty) {
      val interval = PieceFunction.makeInterval(min(from.head, to.head), max(from.head, to.head))
      result += new CLagrange(a.head, b.head, c.head, d.head, interval)
      a = a.tail; b = b.tail; c = c.tail; d = d.tail; to = to.tail; from = from.tail
    }
    result.toList
  }


  @tailrec private def cCoefs(values : List[Tuple2[Double, Double]],
                              result : ListBuffer[List[Double]] = ListBuffer.empty[List[Double]],
                      vector : ListBuffer[Double] = ListBuffer.empty[Double])
  : List[Double] = {
    values match{
      case t1 :: t2 :: t3 :: tail if result.isEmpty => {
        cCoefs(values.tail, result += coefs(hk(t2._1, t1._1), hk(t3._1, t2._1)), vector += vect(t1, t2, t3))
      }
      case t1 :: t2 :: Nil => {
        import approximation.arraygrid._
        val r: List[List[Double]] = {result += coefs(hk(t2._1, t1._1), 0.0)}.result()
        val v: List[Double] = {vector += vect(t1, t2, t2)}.result()
        ListPassion.solve(r, v).head
      }
      case t1 :: t2 :: t3 :: tail => {
        cCoefs(values.tail, result += coefs(hk(t2._1, t1._1), hk(t3._1, t2._1)),
          vector += vect(t1, t2, t3))
      }
    }
  }


  @tailrec private def dCoefs(values : List[Tuple2[Double, Double]], c : List[Double],
                       result : ListBuffer[Double] = ListBuffer.empty[Double]) : List[Double]
  = {
    if((values.size - 2) != c.size) throw new UnsupportedOperationException("Лист values должен быть " +
      "на 2 больше, чем лист с / values list size must be more than c size with 2")
    values match {
      case v1 :: v2 :: tail if result.isEmpty => {
        dCoefs(values.tail, c, result += (c.head - 0.0) / 3 * hk(v2._1, v1._1))
      }
      case v1 :: v2 :: Nil => {
        (result += (0.0 - c.head) / 3 * hk(v2._1, v1._1)).toList
      }
      case v1 :: v2 :: v3 :: tail => {
        dCoefs(values.tail, c.tail, result += (c.head - c.tail.head) / 3 * hk(v2._1, v1._1))
      }
    }
  }

  @tailrec private def bCoefs(values: List[Tuple2[Double, Double]], c : List[Double], d : List[Double],
                      result : ListBuffer[Double] = ListBuffer.empty[Double]) : List[Double] = {
    if(values.tail.nonEmpty){
      val h = hk(values.tail.head._1, values.head._1)
      bCoefs(values.tail, c.tail, d.tail, result += f(values.head, values.tail.head) + c.head * pow(h, 2) - d.head * pow(h, 3))
    }
    else result.toList


  }
  private def aCoefs(values : List[Tuple2[Double, Double]]) : List[Double] = {
    values.drop(1).map(_ _2)
  }


  private def vect(t1 : Tuple2[Double, Double], t2 : Tuple2[Double, Double], t3 : Tuple2[Double, Double]) : Double =
    3*f(t2, t3) - 3*f(t1, t2)

  private def f(x1 : Tuple2[Double, Double], x2 : Tuple2[Double, Double]) : Double = if(x2._1 - x1._1 != 0.0){
    (x2._2 - x1._2) / math.abs(x2._1 - x1._1)}
  else 0.0

  private def hk(x2 : Double, x1 : Double) = x2 - x1

  private def coefs(h1 : Double, h2 : Double) = h1 :: 2 * (h2 + h1) :: h2 :: Nil
}
