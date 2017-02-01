import com.twitter.algebird.Interval

import scala.annotation.tailrec
import scala.math.{abs, signum}
import com.twitter.algebird.Interval._

/** Общий трейт для кусочных функций
  * trait for piecewise functions
  * Created by Даниил on 06.02.2016.
  */
trait PieceFunction extends Serializable {

  val from : Double
  val to : Double

  /** Проверяет, этому ли интервалу принадлежит значение {@code x}
    * Check, are {@code x} belongs to interval of this function
    *
    * @param x значение <code>x</code> / value of <code>x</code>
    * @return если этому, возвращается true, иначе false */
  def ifThis(x: Double): Boolean = {
    if ((x - from) < 0.0 || (x - to) >= 0.0 ) false
    else true
  }

  /** Значение функции в точке {@code x}
    * value of function at {@code x} point
    *
    * @param x точка, в которой ищется значение / point, where is value of function searched */
  def value(x: Double) : Double

  /** Значение производной функции в точке {@code x}
    * value of derivative of function at {@code x} point
    *
    * @param x точка, в которой ищется значение производной / point, where is value of function derivative searched */
  def derivative(x: Double) : Double

  /** Значение интеграла функции в точке {@code x}
    * value of integral of function at {@code x} point
    *
    * @param x точка, в которой ищется значение интеграла функции / point, where is value of function integral searched */
  def integral(x: Double) : Double


  /** Суммирует значения кусочных функций, и возвращает новый сплайн
    *
    *  @param otherFunc другая функция
    *  '''Если функция монотонная, то нужно передавать также монотонную функцию, чтобы полчить првильный ответ'''*/
  def + (otherFunc : A forSome{type A <: PieceFunction})  : B forSome{type B <: PieceFunction}

  /** поднимает функцию на указанную величину, действующую на всём диапазоне
    *
    * @param y значение, на которое прибавляем */
  def + (y : Double) : B forSome{type B <: PieceFunction}

  /** Прибавляет функцию, действующую на всём диапазоне
    *
    * @param func функция, действующая на том же диапазоне.
    * '''Если функция монотонная, то нужно передавать также монотонную функцию, чтобы получить правильный ответ'''*/
  def + (func : (Double) => Double) : B forSome{type B <: PieceFunction}

  /** Умножает точки вфункции на указанную величину
    * @param y величина
    * @return умноженная функция
    */
  def * (y : Double) : B forSome{type B <: PieceFunction}

  /** Определяет значение x при котором две кусочные функции пересекаются. Если фукнции не пересекаются, возвращается
    *  пустой лист
    *  find value of x, where this and other functions are intersected. If functions is not intersected, an empty list
    *  returned
    *
    *  @param other другая функция / other function
    * @return значение x функции */
  def intersect(other: PieceFunction, i : (Double, Double)): List[Double] = {
    intersectedIntervals(other, i).map(tuple =>  bisect(tuple, other))
  }

  /** Определяет значение x, при котором продолжение двух кусочных функций пересекаются. Если нет,
    *  возвращается пустой лист
    * find value of x, where this and other functions are intersected. If functions is not intersected, an empty list
    * returned
    * @param other другая кусочная функция. ''' Должна стоять дальше, чем эта '''
    *              / other piecewise fuction. '''Must stay after this function'''
    *  @return Лист агрументов, где функции пересекаются / list of arguments, where functions are intersected
    */
  def intersectExtrapolated(other : PieceFunction) : List[Double] = {
    val i = captureInterval(other, (minX, other.maxX), 0.01)
    if(i.nonEmpty)
      List(bisect(i.get, other))
    else List.empty
  }

  @tailrec private[this] def captureInterval(other : PieceFunction, i : (Double, Double), prec : Double) : Option[(Double, Double)]= {
    @inline def sig(x : Double) = signum(this.value(x) - other.value(x))
    if(abs(i._2 - i._1) > prec ){
      val center = (i._2+i._1)/2.0
      if(sig(i._1) != sig(center)) captureInterval(other, (i._1, center), prec)
      else if(sig(center) != sig(i._2))  captureInterval(other, (center, i._2), prec)
      else Option.empty
    }
    else Option(i)
  }

  /** Определяет, пересекаются ли две полниноминальные функции
    * determine, are this and other functions are intersected
    *
    * @param other другая функция / other function
    * @return '''true''', если пересекаются. '''false''', если не пересекаются */
  def areIntersectedIntervals(other: PieceFunction): Boolean = {
    if(math.min(to, other.to) > math.max(from, other.from)) true
    else false
  }

  private[this] def intersectedIntervals(other : PieceFunction, x : (Double, Double)) : List[(Double, Double)] = {
    val (x1, x2) = x
    val filteredExt = {extremum ::: other.extremum}.filter(i => valueInsight(i, x))
    val intervalBound = {x1 :: filteredExt ::: x2 :: Nil }.sorted.toSet
    (intervalBound zip {intervalBound drop 1}).filter(i => intersectInsight(other, i)).toList
  }

  @inline private[this] def intersectInsight(other : PieceFunction, interval : (Double, Double)) : Boolean = {
    java.lang.Math.signum(value(interval._1) - other.value(interval._1)) !=
      java.lang.Math.signum(value(interval._2) - other.value(interval._2))
  }

  @inline private[this] def valueInsight(x : Double, interval : (Double, Double)) = {
    x >= interval._1 && x <= interval._2
  }

  @tailrec  private[this] def bisect(interval : Tuple2[Double,Double], other : PieceFunction) : Double = {
    val precision = 0.000001
    @inline def valsDiffer(where : Double) : Double = math.abs(value(where) - other.value(where))
    val center = (interval._1 + interval._2) / 2.0
    val atBounds = List(interval._1, center, interval._2).collect({case x if valsDiffer(x) < precision => x})
    if(atBounds.nonEmpty){
      atBounds.head
    }
    else{
      if(intersectInsight(other, (interval._1, center))) bisect((interval._1, center), other)
      else if(intersectInsight(other, (center, interval._2))) bisect((center, interval._2), other)
      else throw new UnsupportedOperationException(s"Значение должно быть на интервале [${interval._1}, ${interval._2}]")
    }
  }

  /** Величина площади, взятой под графиком на интервале {@code [from;to]}
    * Value of area, takes below spline on interval {@code [from;to]}
    *
    * @param from нижнее значение интервала / lover value of interval
    * @param to верхнее значение интервала / upper value of interval
    * @return Площадь под этим графиком / area below this spline
    * */
  def areaAllIn(from: Double, to: Double): Double = {
    integral(to) - integral(from)
  }

  /** Величина площади, взятой под графиком на интервале {@code [from;_]}
    * Value of area, takes below spline on interval {@code [from;_]}
    *
    * @param from нижнее значение интервала / lower value of interval
    * @return Площадь под этим графиком от <code>from</code> до правой границы интервала
    *         / area below this spline that are taken from <code>from</code> to right boundary of interval
    */
  def areaRightFrom(from: Double): Double = {
    integral(to) - integral(from)
  }

  /** Величина площади, взятой под графиком на интервале {@code [_;to]}
    * Value of area, takes below spline on interval {@code [_;to]}
    *
    * @param to верхнее значение интервала / upper value of interval
    * @return Площадь под этим графиком от лево границы интервала до <code>to</code>
    *         / area below this spline that are taken from lef boundary of interval to <code>to</code>*/
  def areaLeftFrom(to: Double): Double = {
    integral(to) - integral(from)
  }

  def allArea = {
    integral(to) - integral(from)
  }

  /** Экстремум функции
    * extremum of function
    *
    * @return экстремумы функции / extremums of function */
  protected def extremum : List[Double]

  /** Максимум функции / maximum of function
    *
    * @return максимум функции / maximum of function */
  def max = {
    val e = extremum.filter(ifThis(_))
    (from :: to :: e).map(value(_)).max
  }

  /** Значение {@code x} при максимуме функции / value of x, where y is mamximum
    *
    * @return значение x при максимуме функции / value of x, where y is mamximum */
  def maxX = {
    val e = extremum.filter(ifThis(_))
    (from :: to :: e).map(x => (x, value(x))).maxBy(_._2)._1
  }

  /** Минимум функции / minimum of function
    *
    * @return минимум функции / minimum of function */
  def min = {
    val e = extremum.filter(ifThis(_))
    (from :: to :: e).map(value(_)).min
  }

  /** Значение {@code x} при минимуме функции / value of x, where y is minimum
    *
    * @return значение x при минимуме функции / value of x, where y is minimum */
  def minX = {
    val e = extremum.filter(ifThis(_))
    (from :: to :: e).map(x => (x, value(x))).minBy(_._2)._1
  }


  lazy val gnudata = {
    "Температура Точки" :: from + " " + value(from) ::
      (from + to) / 2 + " " + value((from + to) / 2) ::
      from + " " + value(to) :: Nil
  }

  /** Узлы кусочной фукнции. The nodes of piece function */
  def nodes : List[(Double, Double)] = (from, value(from)) :: (to, value(to)) :: Nil
}
