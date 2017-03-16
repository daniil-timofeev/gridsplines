package approximation.passion

import scala.annotation.switch

/**
  * Created by Даниил on 02.03.2017.
  */
class Passion(size: Int) {

  /**  Решает систему уравнений, записанную в виде трёхдиагональной матрицы методом прогонки
    * @param lists массив значений, length x 3
    * @param vector Вектор свободных значений массива
    * @param result массив, в который записываются значения
    */
  def solve(lists : Array[Array[Double]], vector : Array[Double], result: Array[Double]) : Unit = {
    backwardPassion(forwardPassion(lists, vector, result), result)
  }

  private[this] val afterForwardPassion : Array[Array[Double]] = Array.fill(size, 2)(0.0)

  /** Прямая прогонка, во время которой определяются коэффициенты lambda и
    * delta
    * @param array массив значений трёхдиагональной матрицы
    * @param vector вектор свободных членов
    * @return набор пар значений. Первое из них коэффициент delta, второй - lambda */
  protected final def forwardPassion(array : Array[Array[Double]], vector : Array[Double], result : Array[Double]):
  Array[Array[Double]] = {
    var i = 0
    val LAST = result.length - 1
    while(i < result.length){
      val b = array(i)(0)
      val c = array(i)(1)
      val d = array(i)(2)
      val vect = vector(i)
      (i : @switch) match{
        case 0 => {
          forwardFirst(c, d, vect, afterForwardPassion(i))
        }
        case LAST => {
          forwardLast(b, c, d, vect,
            afterForwardPassion(i-1)(0), afterForwardPassion(i-1)(1), afterForwardPassion(i))
        }
        case _ => {
          forwardUnit(b, c, d, vect,
            afterForwardPassion(i-1)(0), afterForwardPassion(i-1)(1), afterForwardPassion(i))
        }
      }
      i += 1
    }
    afterForwardPassion
  }
}
