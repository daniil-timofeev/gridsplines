package approximation

import approximation.passion.{forwardFirst, forwardLast, forwardUnit}
import piecewise.PieceFunction

abstract class Dim[+T <: TypeDir] {

  val t: T
  val low: Double
  val range: Array[Double]
  val upp: Double

  def values: Array[Double] = Array(low) ++ range ++ Array(upp)

  private val coefs: Array[Array[Double]] = t.preDef(low, range, upp, 1.0)
  private val (firstVol, fC) = t.lowerHeatFlow(low, range(0), range(1))
  private val (lastVol, lA) = t.upperHeatFlow(range(range.length - 2), range.last, upp)
  protected val toPassion: Array[Array[Double]] =
    Array.fill(coefs.length)(new Array[Double](2))

  final def coord(i: Int): Double = range(i)

  def result(grid: TwoDGrid.Grid, iter: IterOps): Unit = {

  }

  final
  def first[P <: PieceFunction](time: Double,
            t1: Double,
            t2: Double,
            t3: Double,
            t: Double,
            z0: AlwaysDefinedSpline[P],
            z: AlwaysDefinedSpline[P]): Double = {
    val co0 = passion.takeAverage(t1, t2, z0)
    val co = passion.takeAverage(t2, t3, z)
    val a = coefs(0)(0) * co0
    val c = coefs(0)(1) * co

    val vect: Double = - t / time - a * t1
    forwardFirst(- (a + c) - 1.0 / time, c, vect, toPassion(0))
    co
  }

  final
  def firstHeatFlow[P <: PieceFunction](time: Double,
                    heatFlow: Double,
                    t2: Double,
                    t3: Double,
                    t: Double,
                    conductivity: AlwaysDefinedSpline[P],
                    capacity: AlwaysDefinedSpline[P]): Double = {

    val cap = capacity(t)
    val cond = conductivity.average(t2, t3)
    val vol = firstVol * cap / time
    val c = - cond * fC
    val b = vol - c
    val vect = vol + heatFlow
    val tCond = cond / cap

    forwardFirst(b, c, vect, toPassion(0))
    tCond
  }

  final
  def general[P <: PieceFunction](posAtLayer: Int,
              time: Double,
              t2: Double,
              t3: Double,
              t: Double,
              co0: Double,
              z: AlwaysDefinedSpline[P]): Double = {
    val co = passion.takeAverage(t2, t3, z)
    val a = coefs(posAtLayer)(0) * co0
    val c = coefs(posAtLayer)(1) * co
    val vect = - t / time
    forwardUnit(a, - (a + c) - 1.0 / time, c, vect,
      toPassion(posAtLayer - 1)(0),
      toPassion(posAtLayer - 1)(1),
      toPassion(posAtLayer))
    co
  }

  final
  def last[P <: PieceFunction](
           posAtLayer: Int,
           time: Double,
           t2: Double,
           t3: Double,
           t: Double,
           co0: Double,
           z: AlwaysDefinedSpline[P]): Unit = {
    val co = passion.takeAverage(t2, t3, z)
    val a = coefs(posAtLayer)(0) * co0
    val c = coefs(posAtLayer)(1) * co
    val vect = - t / time - c * t3

    forwardLast(a, - (a + c) - 1.0 / time, vect,
      toPassion(posAtLayer - 1)(0),
      toPassion(posAtLayer - 1)(1),
      toPassion(posAtLayer))
  }

  final
  def lastHeatFlow[P <: PieceFunction](posAtLayer: Int,
                    time: Double,
                    heatFlow: Double,
                    t1: Double,
                    t2: Double,
                    t: Double,
                    conductivity: AlwaysDefinedSpline[P],
                    capacity: AlwaysDefinedSpline[P]): Double = {

    val cap = capacity(t)
    val cond = conductivity.average(t1, t2)
    val vol = cap * lastVol / time
    val a = - cond * lA
    val b = vol - a
    val vect = vol - heatFlow
    val tCond = cond / cap

    forwardLast(a, b, vect,
      toPassion(posAtLayer - 1)(0),
      toPassion(posAtLayer - 1)(1),
      toPassion(posAtLayer))
    tCond
  }

  def update(grid: TwoDGrid.Grid, pos: Int, colsNum: Int): Unit

}
