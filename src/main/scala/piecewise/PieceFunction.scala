package piecewise
import java.util.Objects

import com.twitter.algebird.Interval.MaybeEmpty.{NotSoEmpty, SoEmpty}
import com.twitter.algebird._

import scala.annotation.{switch, tailrec}
import scala.math.{abs, signum}

import com.twitter.algebird.Interval._

/** Общий трейт для кусочных функций
  * trait for piecewise functions
  * Created by Даниил on 06.02.2016.
  */
abstract class PieceFunction{

  /** Значение функции в точке {@code x}
    * v of function at {@code x} point
    *
    * @param x точка, в которой ищется значение / point, where is yL of function searched */
  def apply(x: Double): Double

  /** Значение производной функции в точке {@code x}
    * v of derivative of function at {@code x} point
    *
    * @param x точка, в которой ищется значение производной / point, where is yL of function derivative searched */
  def derivative(x: Double): Double

  /** Значение производной функции в точке {@code x}
    * v of derivative of function at {@code x} point
    *
    * @param x точка, в которой ищется значение производной / point, where is yL of function derivative searched */
  final def der(x: Double): Double = derivative(x)

  /** Значение интеграла функции в точке {@code x}
    * v of integral of function at {@code x} point
    *
    * @param x точка, в которой ищется значение интеграла функции / point, where is yL of function integral searched */
  def integral(x: Double): Double

  /** Значение интеграла функции в точке {@code x}
    * v of integral of function at {@code x} point
    *
    * @param x точка, в которой ищется значение интеграла функции / point, where is yL of function integral searched */
  final def int(x: Double): Double = integral(x)

  /** Суммирует значения кусочных функций, и возвращает новый сплайн
    *
    *   otherFunc другая функция
    *  '''Если функция монотонная, то нужно передавать также монотонную функцию, чтобы полчить првильный ответ'''*/
  // def + (otherFunc: A forSome{type A <: PieceFunction}): B forSome{type B <: PieceFunction}

  /** Поднимает функцию на указанную величину, действующую на всём диапазоне
    *
    *  y значение, на которое прибавляем */
  // def + (y : Double) : B forSome{type B <: PieceFunction}

  /** Прибавляет функцию, действующую на всём диапазоне
    *
    *  func функция, действующая на том же диапазоне.
    * '''Если функция монотонная, то нужно передавать также монотонную функцию, чтобы получить правильный ответ'''*/
  // def + (func : (Double) => Double) : B forSome{type B <: PieceFunction}

  /** Умножает точки вфункции на указанную величину
    *  y величина
    * @return умноженная функция
    */
  // def * (y : Double) : B forSome{type B <: PieceFunction}

  /** Определяет значение x при котором две кусочные функции пересекаются. Если фукнции не пересекаются, возвращается
    *  пустой лист
    *  find yL of x, where this and other functions are intersected. If functions is not intersected, an empty list
    *  returned
    *
    *  @param other другая функция / other function
    * @return значение x функции */
  def intersect(other: PieceFunction, i : (Double, Double)): List[Double] = {
    intersectedIntervals(other, i).map(tuple =>  bisect(tuple, other))
  }


  @tailrec private[this] def captureInterval(other : PieceFunction, i : (Double, Double), prec : Double) : Option[(Double, Double)]= {
    @inline def sig(x : Double) = signum(this(x) - other(x))
    if(abs(i._2 - i._1) > prec ){
      val center = (i._2+i._1)/2.0
      if(sig(i._1) != sig(center)) captureInterval(other, (i._1, center), prec)
      else if(sig(center) != sig(i._2))  captureInterval(other, (center, i._2), prec)
      else Option.empty
    }
    else Option(i)
  }

  private[this] def intersectedIntervals(other : PieceFunction, x : (Double, Double)) : List[(Double, Double)] = {
    val (x1, x2) = x
    val filteredExt = {extremum ::: other.extremum}.filter(i => valueInsight(i, x))
    val intervalBound = {x1 :: filteredExt ::: x2 :: Nil }.sorted.toSet
    (intervalBound zip {intervalBound drop 1}).filter(i => intersectInsight(other, i)).toList
  }

  @inline private[this] def intersectInsight(other : PieceFunction, interval : (Double, Double)) : Boolean = {
    java.lang.Math.signum(apply(interval._1) - other(interval._1)) !=
      java.lang.Math.signum(apply(interval._2) - other(interval._2))
  }

  @inline private[this] def valueInsight(x: Double, interval: (Double, Double)) = {
    x >= interval._1 && x <= interval._2
  }

  @tailrec  private[this] def bisect(interval: Tuple2[Double,Double], other : PieceFunction) : Double = {
    val precision = 0.000001
    @inline def valsDiffer(where : Double) : Double = math.abs(apply(where) - other(where))
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
    * @param lower нижнее значение интервала / lover yL of interval
    * @param upper верхнее значение интервала / upper yL of interval
    * @return Площадь под этим графиком / area below this spline
    * */
  def area(lower: Double, upper: Double): Double = {
    integral(upper) - integral(lower)
  }

  /** Экстремум функции `x`
    * Extremum of function `x`
    *
    * @return экстремумы функции / extremums of function */
  protected def extremum : List[Double]


  /** Максимум функции / maximum of function
    *
    * @return максимум функции / maximum of function */
  def max: Double = {
  ???
  }

  /** Значение {@code x} при максимуме функции / yL of x, where y is mamximum
    *
    * @return значение x при максимуме функции / yL of x, where y is mamximum */
  def maxX: Double = {
    ???
    }



  /** Минимум функции / minimum of function
    *
    * @return минимум функции / minimum of function */
  def min: Double = {
    ???
  }

  /** Значение {@code x} при минимуме функции / yL of x, where y is minimum
    *
    * @return значение x при минимуме функции / yL of x, where y is minimum */
  def minX = {
   ???
  }
}
object PieceFunction{

  def makeInterval(low: Double, up: Double): Intersection[InclusiveLower, ExclusiveUpper, Double] = {
    Interval.leftClosedRightOpen(low, up) match{
      case NotSoEmpty(interval) => interval
      case SoEmpty() => throw new IllegalArgumentException("Interval must not be empty")
    }
  }

  def sliceUpper(value: Double,
                 interval: Intersection[InclusiveLower, ExclusiveUpper, Double])
  : Intersection[InclusiveLower, ExclusiveUpper, Double] = {
    Intersection.apply(interval.lower, ExclusiveUpper(value))
  }

  def sliceLower(value: Double,
                 interval: Intersection[InclusiveLower, ExclusiveUpper, Double])
  : Intersection[InclusiveLower, ExclusiveUpper, Double] = {
    Intersection.apply(InclusiveLower(value), interval.upper)
  }


  /** General rule of Gorner for polynomial function valu finder
    * @param x argument position
    * @param a coefficients. a(0)at maximum extent, a(size - 1) at 0 extent
    * @return polynominal root
    * */
  final def ruleOfGorner(x: Double, a: Double*): Double = {
    var res = a.last
    var i = a.size - 2
    while(i != -1){
      res = res*x + a(i)
      i -= 1
    }
    res
  }

  final def polynomial(x: Double, a: Double*): Double = {
  var i = 0
  var res = 0.0
  while(i != a.size){
    res += math.pow(x, i) * a(i)
    i += 1
  }
    res
  }


  /** General rule of Gorner for quad degree polynomial function v finder
    * @param x argument position
    * @param a4 coef at x^4^
    * @param a3 coef at x^3^
    * @param a2 coef at x^2^
    * @param a1 coef at x^1^
    * @param a0 coef at x^0^
    * @return polynominal root
    * */
  final def quadRuleOfGorner(x: Double, a0: Double, a1: Double, a2: Double, a3: Double, a4: Double): Double = {
    (((a4 * x + a3) * x + a2) * x + a1) * x + a0
  }


  /** General rule of Gorner for third degree polynomial function v finder
    * @param x argument position
    * @param a3 coef at x^3^
    * @param a2 coef at x^2^
    * @param a1 coef at x^1^
    * @param a0 coef at x^0^
    * @return polynominal root
    * */
  final def cubicRuleOfHorner(x: Double, a0: Double, a1: Double, a2: Double, a3: Double): Double = {
    ((a3 * x + a2) * x + a1) * x + a0
  }

  final def cubicHornerIntegral(x: Double, a0: Double, a1: Double, a2: Double, a3: Double): Double = {
    quadRuleOfGorner(x, 0.0, a0, 0.5 * a1 , a2 / 3.0, 0.25 * a3)
  }

  final def cubicHornerDerivative(x: Double, a0: Double, a1: Double, a2: Double, a3: Double): Double = {
    quadraticRuleOfHorner(x, a1, 2.0 * a2, 3.0 * a3)
  }

  /** General rule of Gorner for second degree polynomial function v finder
    * @param x argument position
    * @param a2 coef at x^2^
    * @param a1 coef at x^1^
    * @param a0 coef at x^0^
    * @return polynominal root
    * */
  final def quadraticRuleOfHorner(x: Double, a0: Double, a1: Double, a2: Double): Double = {
    (a2 * x + a1) * x + a0
  }

  final def quadraticHornerIntegral(x: Double, a0: Double, a1: Double, a2: Double): Double = {
    cubicRuleOfHorner(x, 0.0, a0, a2 / 2.0, a2 / 3.0)
  }

  final def quadraticHornerDerivative(x: Double, a0: Double, a1: Double, a2: Double): Double = {
    atLine(x, a1, 2.0 * a2)
  }

  final def atLine(x: Double, a0: Double, a1: Double) = {
    a1 * x + a0
  }

  final def atLineIntegral(x: Double, a0: Double, a1: Double) = {
    quadraticRuleOfHorner(x, 0.0, a0, a1 / 2.0)
  }

  def interpolate(x1: Double, x2 : Double, y1 : Double, y2 : Double, x : Double) = {
    if(Objects.equals(x2-x1, 0.0)) y1
    else y1 + (y2 - y1) / (x2 - x1) * (x - x1)
  }

  /** Интерполирует значение между двумя ближайшими точками к значению в коллекции
    * @param value значений
    * @param coll коллекция
    * @param func значение, между которыми интеполируем */
  def int2Points(value : Double, coll : List[Double], func : Array[Double]) = {
    val solved = interpolate2(value, coll)
    interpolate(solved.head._1, solved.last._1, func(solved.head._2), func(solved.last._2), value)
  }

  @tailrec
  def interpolate2(value : Double, coll : List[Double], firstIndex : Int = 0) : List[Tuple2[Double, Int]] = {
    val size = coll.size
    @inline val solve = (x : List[Double]) => signum(x.last - value) != signum(x.head - value)
    size match {
      case 2 => {
        val a = coll.head
        val b = coll.tail.head
        value match {
          case `a` => List(Tuple2(a, firstIndex), Tuple2(a, firstIndex))
          case `b` => List(Tuple2(b, firstIndex + 1) , Tuple2(b, firstIndex + 1))
          case _ => List((a, firstIndex), (b, firstIndex + 1))
        }
      }
      case 3 => {
        val a = coll.head
        val b = coll.tail.head
        val c = coll.tail.tail.head
        val first = coll.dropRight(1)
        value match{
          case `a` => List((a, firstIndex), (b, firstIndex))
          case `b` => List((b, firstIndex + 1), (b, firstIndex + 1))
          case `c` => List((c, firstIndex + 2), (c, firstIndex + 2))
          case _ if(solve(List(a, b))) => List((a, firstIndex), (b, firstIndex + 1))
          case _ => List((b, firstIndex + 1),(c, firstIndex + 2))
        }
      }
      case _ => {
        val first = coll.dropRight(size/2)
        if(solve(first)){
          interpolate2(value, first, firstIndex)}
        else if(solve(coll.drop(size/2 + 1))){interpolate2(value, coll.drop(size/2), firstIndex + size/2 - 1)}
        else {
          if(coll.head < coll.tail.head){
            if(coll.head > value) List((coll.head, 0), (coll.head, 0))
            else List((coll.last, size - 1), (coll.last, size - 1))
          } else if (coll.head < value) {List((coll.head, 0),(coll.head, 0))
          } else List((coll.last, size - 1), (coll.last, size - 1))
        }
      }
    }}

}
