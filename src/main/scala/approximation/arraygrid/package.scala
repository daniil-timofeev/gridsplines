package approximation

import scala.math.pow

/**
  * Created by Даниил on 02.03.2017.
  */
package object arraygrid {

  def makeOrthogonalMatrix(upperBoundCoord: Double,
                           orthogonalCoords: Array[Double],
                           lowerBoundCoord: Double,
                           sigma: Double = 1.0): Array[Array[Double]] = {

    /**
      * Генерирует лист коэффициентов а и с (без учёта температуропроводности)
      * трёхдиагональной матрицы. Коэффициент w далее находится как -(alpha1 x a + alpha2 x w)
      *
      * @param l1 координата r1 шаблона
      * @param l2 координата r2 шаблона
      * @param l3 координата r3 шаблона
      * @return List(a, c)
      **/
    @inline def listOfCoef(l1: Double, l2: Double, l3: Double) = {
      val average = ah(l1, l2, l3)
      val a = coef(l1, l2, average)
      val c = coef(l2, l3, average)
      Array(a, c)
    }

    /** Генерирует значение коэффициентa трёхдиагональной матрицы на шаблоне
      *
      * @param lOne координата одного узла на шаблоне
      * @param lTwo координата следующего узла на шаблоне
      * @param h    объём средней ячейки шаблона
      * @return коэффициет a или с, без учёта коэффициента температуропроводности
      */
    @inline def coef(lOne: Double, lTwo: Double, h: Double): Double = {
      sigma / h / dh(lOne, lTwo)
    }

    /** Ширина объёма, который представляет узел
      *
      * @param lOne   координата первого узла
      * @param lTwo   координата среднего узла
      * @param lThree координата третьего узла
      * @return Ширина объёма, который представляет узел, м
      */
    @inline def ah(lOne: Double, lTwo: Double, lThree: Double) = {
      (lThree + lTwo) / 2.0 - (lOne + lTwo) / 2.0
    }
    val LAST = orthogonalCoords.length - 1
    val result =
      for (i <- orthogonalCoords.indices) yield {
        i match {
          case 0 => {
            val l1 = upperBoundCoord
            val l2 = orthogonalCoords(i)
            val l3 = orthogonalCoords(i + 1)
            listOfCoef(l1, l2, l3)
          }
          case LAST => {
            val l1 = orthogonalCoords(i - 1)
            val l2 = orthogonalCoords(i)
            val l3 = lowerBoundCoord
            listOfCoef(l1, l2, l3)
          }
          case _ => {
            val l1 = orthogonalCoords(i - 1)
            val l2 = orthogonalCoords(i)
            val l3 = orthogonalCoords(i + 1)
            listOfCoef(l1, l2, l3)
          }
        }
      }
    result.toArray
  }

  /** Переопределяет предположение о темепературе на следующем шаге
    * @param oldVal температуры до итерирования
    * @param newVal температуры после итерирования
    * @return предполагаемая температура на следующем шаге
    */
  @inline
  final def aVal(oldVal: Double, newVal: Double): Double = {
    newVal + (newVal - oldVal) / 3.0
  }

  @inline
  final def midVal(oldVal: Double, newVal: Double): Double = {
    (oldVal + newVal) / 2.0
  }
  /** Шаг по расстоянию между двумя узлами
    *
    * @param l1 координата первого узла, м
    * @param l2 координата следующего узла, м
    * @return шаг по расстоянию, м
    */
  @inline final def dh(l1 : Double, l2 : Double) = {
    l2 - l1
  }

  def makeRadialMatrix(leftBoundCoord: Double,
                       radialCoords: Array[Double],
                       rightBoundCoord: Double, sigma: Double = 1.0): Array[Array[Double]] = {

    /**
      * Генерирует лист коэффициентов а и с (без учёта температуропроводности)
      * трёхдиагональной матрицы. Коэффициент w далее находится как -(alpha1 x a + alpha2 x w)
      *
      * @param r1 координата r1 шаблона
      * @param r2 координата r2 шаблона
      * @param r3 координата r3 шаблона
      * @return List(a, c)
      */
    @inline def listOfCoef(r1 : Double, r2 : Double, r3 : Double) = {
      val volume = v(r1, r2, r3)
      val a = coef(r1, r2, volume); val c = coef(r2, r3, volume)
      Array(a, c)
    }

    /** Значение кооординаты между двумя узлами
      *
      * @param rOne координата первого узла, м
      * @param rTwo координата второго узал, м
      * @return середина, м
      */
    @inline def rAtHalf(rOne : Double, rTwo : Double) = {
      (rOne + rTwo) / 2.0
    }

    /** Объём ячейки шаблона высотой один метр, которую пердставляет средний узел
      *
      * @param rOne координата первого узла
      * @param rTwo координата среднего узла
      * @param rThree координата третьего узла
      * @return объём ячейки, м3/м
      */
    @inline def v(rOne : Double, rTwo : Double, rThree : Double) = {
      (pow(rAtHalf(rTwo, rThree), 2.0) - pow(rAtHalf(rOne, rTwo), 2.0)) / 2.0
    }

    /** Генерирует значение коэффициент трёхдиагональной матрицы на шаблоне

      * @param rOne координата одного узла на шаблоне
      * @param rTwo координата следующего узла на шаблоне
      * @param v объём средней ячейки шаблона
      * @return коэффициет a или с, без учёта коэффициента температуропроводности
      * **/
    @inline def coef(rOne : Double, rTwo : Double, v : Double) : Double= {
      sigma / v * rAtHalf(rOne, rTwo) / dh(rOne, rTwo)
    }

    val LAST = radialCoords.length - 1
    val result =
      for (i <- radialCoords.indices) yield {
        i match {
          case 0 => {
            val r1 = leftBoundCoord
            val r2 = radialCoords(i)
            val r3 = radialCoords(i + 1)
            listOfCoef(r1, r2, r3)
          }
          case LAST => {
            val r1 = radialCoords(i - 1)
            val r2 = radialCoords(i)
            val r3 = rightBoundCoord
            listOfCoef(r1, r2, r3)
          }
          case _ => {
            val r1 = radialCoords(i - 1)
            val r2 = radialCoords(i)
            val r3 = radialCoords(i + 1)
            listOfCoef(r1, r2, r3)
          }
        }
      }
    result.toArray
  }




}
