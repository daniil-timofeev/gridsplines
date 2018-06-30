package piecewise
import java.util.Objects

import scala.annotation.tailrec
import scala.math.{abs, signum}

/** Trait for piecewise functions
  * Created by daniil-timofeev on 06.02.2016.
  */
abstract class PieceFunction{

  /** The value of function at `x` argument
    *
    * @param x function argument
    */
  def apply(x: Double): Double

  /** The value of derivative of function at `x` argument
    *
    * @param x function argument
    */
  def derivative(x: Double): Double

  /** The value of derivative of function at `x` argument
    *
    * @param x function argument
    */
  final def der(x: Double): Double = derivative(x)

  /** The value of integral of function at `x` argument
    *
    * @param x function argument
    * */
  def integral(x: Double): Double

  /** The value of integral of function at `x` argument
    *
    * @param x function argument
    * */
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

  /** Find yL of x, where this and other functions are intersected. If functions is not intersected, an empty list
    *  returned
    *
    *  @param other другая функция / other function
    * @return значение x функции */
  def intersect(other: PieceFunction, i: (Double, Double)): List[Double] = {
    intersectedIntervals(other, i).map(tuple => bisect(tuple, other))
  }


  @tailrec private[this] def captureInterval(other: PieceFunction, i: (Double, Double),
                                             prec: Double) : Option[(Double, Double)] = {
    @inline def sig(x : Double) = signum(this(x) - other(x))
    if(abs(i._2 - i._1) > prec ){
      val center = (i._2+i._1)/2.0
      if(sig(i._1) != sig(center)) captureInterval(other, (i._1, center), prec)
      else if(sig(center) != sig(i._2))  captureInterval(other, (center, i._2), prec)
      else Option.empty
    }
    else Option(i)
  }

  private[this] def intersectedIntervals(other: PieceFunction,
                                         x: (Double, Double)): List[(Double, Double)] = {
    val (x1, x2) = x
    val filteredExt = {extremum(x1, x2) ::: other.extremum(x1, x2)}.filter(i => valueInsight(i, x))
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

  /** Value of area, takes below spline on interval `[from;to]`
    *
    * @param lower lover yL of the interval
    * @param upper upper yL of the interval
    * @return an area below this spline
    * */
  def area(lower: Double, upper: Double): Double = {
    integral(upper) - integral(lower)
  }

  /**
    * Extremum of the functions at bounds `x`
    *
    * @return extremums of function */
  protected def extremum(low: Double, upp: Double): List[Double]

}
object PieceFunction{

  /** General rule of Gorner for polynomial function valu finder
    * @param x argument position
    * @param a coefficients. a(0) at 0, a(size - 1) at maximum extent
    * @return polynominal root
    * */
  final def ruleOfHorner(x: Double, a: Double*): Double = {
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


  /** General Horner's rule which finds a value of the quad degree polynomial function
    * @param x argument position
    * @param a4 coef at x^4^
    * @param a3 coef at x^3^
    * @param a2 coef at x^2^
    * @param a1 coef at x^1^
    * @param a0 coef at x^0^
    * @return polynominal root
    * */
  final def quadRuleOfHorner(x: Double, a0: Double, a1: Double, a2: Double, a3: Double, a4: Double): Double = {
    (((a4 * x + a3) * x + a2) * x + a1) * x + a0
  }


  /** General Horner's rule which finds a value of the third degree polynomial function
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
    quadRuleOfHorner(x, 0.0, a0, 0.5 * a1 , a2 / 3.0, 0.25 * a3)
  }

  final def cubicHornerDerivative(x: Double, a0: Double, a1: Double, a2: Double, a3: Double): Double = {
    quadraticRuleOfHorner(x, a1, 2.0 * a2, 3.0 * a3)
  }

  /** General Horner's rule which finds a value of the second degree polynomial function
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

  def iteratePoints[R](values: Iterator[(Double, Double)],
                       whenEmpty: => Iterator[R],
                       whenOne: (Double, Double) => R,
                       whenTwo: ((Double, Double), (Double, Double)) => R,
                       whenThree: ((Double, Double),
                                   (Double, Double),
                                   (Double, Double)) => R,
                       whenMore: (((Double, Double),
                                   (Double, Double),
                                   (Double, Double)) => R)): Iterator[R] = {

    val builder = collection.mutable.Buffer.apply[R]()
    if (values.hasNext){
      var v1 = values.next()
      if (values.isEmpty) {
        builder += whenOne(v1._1, v1._2)
        builder.iterator
      }
      else {
        var v2 = values.next()
        if (values.isEmpty) {
          builder += whenTwo(v1, v2)
          builder.iterator
        }
        else {
          var v3 = values.next()
          if (values.isEmpty){
            builder += whenThree(v1, v2, v3)
            builder.iterator
          }
          else {
            builder +=  whenMore(v1, v2, v3)
            while (values.hasNext){
              v3 = v2
              v2 = v1
              v1 = values.next()
              builder += whenMore(v1, v2, v3)
            }
            builder.iterator
          }
        }
      }
    }
    else whenEmpty
  }

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
