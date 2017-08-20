package approximation

import approximation.passion.{cons, forwardFirst, forwardLast, forwardUnit}
import piecewise.{PieceFunction, Spline}

abstract class Dim[T <: TypeDir] {

  val t: T
  val low: Double
  val range: Array[Double]
  val upp: Double

  private val coefs: Array[Array[Double]] = t.preDef(low, range, upp, 1.0)
  private val toPassion: Array[Array[Double]] =
    Array.fill(coefs.length)(new Array[Double](2))

  final def coord(i: Int): Double = range(i)

  def result(grid: TwoDGrid.Grid, iter: IterOps): Unit = {

  }

  final
  def first(time: Double,
            t1: Double,
            t2: Double,
            t3: Double,
            t: Double,
            z0: Spline[PieceFunction],
            z: Spline[PieceFunction]): Double = {
    val co0 = passion.conducitity(t1, t2, z0)
    val co = passion.conducitity(t2, t3, z)
    val a = coefs(0)(0) * co0
    val c = coefs(0)(1) * co

    val vect: Double = - t / time - a * t1
    forwardFirst(- (a + c) - 1.0 / time, c, vect, toPassion(0))
    co
  }

  final
  def general(iter: IterOps,
              time: Double,
              t2: Double,
              t3: Double,
              t: Double,
              co0: Double,
              z: Spline[PieceFunction]): Double = {
    val idx = iter.posAtLayer
    val co = passion.conducitity(t2, t3, z)
    val a = coefs(idx)(0) * co0
    val c = coefs(idx)(1) * co
    val vect = - t / time
    forwardUnit(a, - (a + c) - 1.0 / time, c, vect,
      toPassion(idx - 1)(0), toPassion(idx - 1)(1), toPassion(idx))
    co
  }

  final
  def last(iter: IterOps,
           time: Double,
           t2: Double,
           t3: Double,
           t: Double,
           co0: Double,
           z: Spline[PieceFunction]): Unit = {
    val idx = iter.posAtLayer
    val co = passion.conducitity(t2, t3, z)
    val a = coefs(idx)(0) * co0
    val c = coefs(idx)(1) * co
    val vect = - t / time - c * t3

    forwardLast(a, - (a + c) - 1.0 / time, c, vect,
      toPassion(idx - 1)(0), toPassion(idx - 1)(1), toPassion(idx))
  }
  final
  def update(grid: TwoDGrid.Grid, iter: IterOps): Unit = {
    passion.backwardPassion(toPassion, grid, iter)
  }

}
