package approximation

import scala.math.pow

/**
  *
  */
package object arraygrid {

  /** A Width of the volume which presents the node
    *
    * @param lOne   coordinate of the prevous node
    * @param lTwo   coordinate of the current node
    * @param lThree coordinate of the next node
    * @return A Width of the volume which presents the node, m
    */
  @inline def ah(lOne: Double, lTwo: Double, lThree: Double) = {
    (lThree + lTwo) / 2.0 - (lOne + lTwo) / 2.0
  }

  def makeOrthogonalMatrix(upperBoundCoord: Double,
                           orthogonalCoords: Array[Double],
                           lowerBoundCoord: Double,
                           sigma: Double = 1.0): Array[Array[Double]] = {

    /** Provide a list of the coefficients `a` and `c` (without taking into account the temperature conductivity)
      * for the current node. This coefficients will be used in bound diagonal at the three diagonal matrix.
      * Central diagonal will be calculated as `-(alpha1 x a + alpha2 x w)`
      *
      * @param l1 prevous node coordinate, m;
      * @param l2 current node coordinate r2, m;
      * @param l3 next node coordinate r3, m.
      * @return List(a, c)
      **/
    @inline def listOfCoef(l1: Double, l2: Double, l3: Double) = {
      val average = ah(l1, l2, l3)
      val a = coef(l1, l2, average)
      val c = coef(l2, l3, average)
      Array(a, c)
    }

    /** Provide a value of coefficient of the tridiagonal matrix
      *
      * @param lOne coordinate of the node
      * @param lTwo coordinate of the next node
      * @param h    average node width
      *
      */
    @inline def coef(lOne: Double, lTwo: Double, h: Double): Double = {
      sigma / h / dh(lOne, lTwo)
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

  /** Predicts the temperature at the next step
    * @param oldVal old temperature values
    * @param newVal current temperature value
    */
  @inline
  final def aVal(oldVal: Double, newVal: Double): Double = {
    newVal + (newVal - oldVal) / 3.0
  }

  @inline
  final def midVal(oldVal: Double, newVal: Double): Double = {
    (oldVal + newVal) / 2.0
  }
  /** A distance between the two adjacent nodes
    * @param l1 position of the first node, m
    * @param l2 position of the second node, m
    * @return a distance between nodes, m
    */
  @inline final def dh(l1 : Double, l2 : Double) = {
    l2 - l1
  }


  def generalRadialCoefs(lowR: Double,
                         midR: Double,
                         maxR: Double,
                         sigma: Double = 1.0): Array[Double] = {

    @inline
    def coef(rOne: Double, rTwo: Double): Double= {
      rAtHalf(rOne, rTwo) / dh(rOne, rTwo)
    }

    val volume = vol(lowR, midR, maxR)
    val a = coef(lowR, midR)
    val b = coef(midR, maxR)

    Array(volume / sigma, a, b)
  }

  def makeRadialMatrix(leftBoundCoord: Double,
                       radialCoords: Array[Double],
                       rightBoundCoord: Double, sigma: Double = 1.0)
  : Array[Array[Double]] = {

    /** Provide a list of the coefficients `a` and `c` (without taking into account the temperature conductivity)
      * for the current node. This coefficients will be used in bound diagonal at the three diagonal matrix.
      * Central diagonal will be calculated as `-(alpha1 x a + alpha2 x w)`
      *
      * @param r1 prevous node coordinate, m;
      * @param r2 current node coordinate r2, m;
      * @param r3 next node coordinate r3, m.
      * @return List(a, c)
      **/
    @inline def listOfCoef(r1 : Double, r2 : Double, r3 : Double) = {
      val volume = vol(r1, r2, r3)
      val a = coef(r1, r2, volume); val c = coef(r2, r3, volume)
      Array(a, c)
    }

    /** Provide a value of coefficient of the tridiagonal matrix
      *
      * @param rOne coordinate of the node
      * @param rTwo coordinate of the next node
      * @param v    average node volume
      *
      */
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


  /** Position value between the two adjacent nodes
    *
    * @param rOne right node position, m
    * @param rTwo left node position, m
    * @return center, m
    */
  @inline def rAtHalf(rOne : Double, rTwo : Double) = {
    (rOne + rTwo) / 2.0
  }

  /** Node volume with one meter height, which presents the central node
    *
    * @param rOne prevous node position
    * @param rTwo current node position
    * @param rThree next node position
    * @return node volume, m3/m
    */
  @inline def vol(rOne : Double, rTwo : Double, rThree : Double) = {
    (pow(rAtHalf(rTwo, rThree), 2.0) - pow(rAtHalf(rOne, rTwo), 2.0)) / 2.0
  }



}
