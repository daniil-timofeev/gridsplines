package piecewise.utils

import java.text.NumberFormat
import java.util.Locale

import scala.annotation.tailrec
import scala.math._
object ListPassion {


  final def solve(lists : List[(Double, Double, Double)],
                  vector: List[Double]): List[List[Double]] = {
    backwardPassion(forwardPassion(lists, vector))
  }

  /** Прямая прогонка, во время которой определяются коэффициенты lambda и
    * delta
    * @param array массив значений трёхдиагональной матрицы
    * @param vector вектор свободных членов
    * @return набор пар значений. Первое из них коэффициент delta, второй - lambda */
  final def forwardPassion(array: List[(Double, Double, Double)],
                           vector: List[Double]): List[(Double, Double)] = {

    val (b, c, d) = array(0)
    val list = forwardFirst(c, d, vector.head)
    val (delta, lambda) = list

    @inline @tailrec def fow(array: List[(Double, Double, Double)],
                             vector: List[Double],
                           lists: List[(Double, Double)] =
                             List.empty[(Double, Double)],
                             delta: Double,
                             lambda: Double) :
    List[(Double, Double)] = {

      val row = array.head
      val (b, c, d) = row

      if(array.tail.nonEmpty){
        val list = forwardUnit(b, c, d, vector.head, delta, lambda)
        fow(array.tail, vector.tail, list :: lists, list._1, list._2)
      }
      else{
        val list = forwardLast(b, c, d, vector.head, delta, lambda)
        list :: lists
      }
    }
    fow(array.tail, vector.tail, list :: Nil, delta, lambda)
  }


  private val format3 = NumberFormat.getNumberInstance(Locale.ROOT)
  format3.setMaximumFractionDigits(3)

  @inline private[this] def del(c : Double, b : Double, delta : Double) : Double = c + b*delta

  @inline private[this] def lam(r : Double, b : Double, lambda : Double, del : Double) : Double = (r - b * lambda) / del

  @inline final def forwardUnit(b : Double, c : Double, d : Double,
                                  vect : Double, delta : Double, lambda : Double) : (Double, Double) = {

    val format3 = NumberFormat.getNumberInstance(Locale.ROOT)
    format3.setMaximumFractionDigits(3)
    // Корректность прогонки
    assert(abs(c) >= abs(d) + abs(b), "Прогонка некорректна и/или неустойчива." +
      " w: "+ format3.format(b)  +", с: " + format3.format(c, 3) + ", d: " + format3.format(d, 3) +
      ". Вектор: " + format3.format(vect, 3) + ".")
    val bigDelta = del(c, b, delta)
    (- d / bigDelta, lam(vect, b, lambda, bigDelta))
  }

  /** Первый крайний шаблон прогонки
    * @param c коэффициент по центру
    * @param d коэффициент справа */
  @inline final def forwardFirst(c: Double, d: Double, vect: Double): (Double, Double) = {
    (- d / c,  vect / c)
  }

  @inline final def forwardLast(b: Double, c: Double, d: Double,
                                  vect: Double,
                                delta: Double, lambda: Double): (Double, Double) = {
    val bigDelta = del(c, b, delta)
    val nextLambda = lam(vect, b, lambda, bigDelta)
    (0.0, nextLambda)
  }


  /** Обратную прогонку, для нахождения решений уравнения
    * @param coefficients лист коэффициентов delta :: lambda :: Nil
    */
   final def backwardPassion(coefficients : List[(Double, Double)]) : List[List[Double]] = {

    @inline def point(list: (Double, Double), prev: Double): Double = {
    val(delta, lambda) = list
      delta * prev + lambda
    }

    @tailrec def back(coefs : List[(Double, Double)],
                      result : List[Double],
                      prev : Double, last : Double)
    : List[List[Double]] = {
      if(coefs.isEmpty)  List(result, List(last))
      else {
        val x = point(coefs.head, prev)
        back(coefs.tail, x :: result, x, last)
      }
    }

    val x = point(coefficients.head, 0.0)
    back(coefficients.tail, x :: Nil, x, x)
  }
}
