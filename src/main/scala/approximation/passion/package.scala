package approximation

import piecewise._

import scala.math.abs
/**
  * Created by Даниил on 02.03.2017.
  */
package object passion{


  /** Завершает конструирование элемента матрицы интегро-нтерполяционного метода (умножая его на температуропроводность)
    *
    * @param t1 температура на новом временном слое узла 1, град. С
    * @param t2 температура на новом временном слое узла 2, град. С
    * @param c предрассчитанный коэффициент без теплофизических свойств
    * @param z график зависимости температуропроводности от температуры для данной глубины
    * @return коэффициент трёхдиагональной матрицы (а или с. Коэффициент w, который на диагонали рассчитывается как
    *         <code>val w = -(a+w)- 1.0/tau)</code>, где tau - время шага, сек.
    */
  @inline
   final def cons(t1: Double, t2: Double, c: Double,
                  z: AlwaysDefinedSpline[PieceFunction]): Double = {
    val cond = z((t1 + t2) / 2.0)
    cond * c
  }

  @inline
  final def cons(c: Double, z: Double): Double = {
    c * z
  }

  @inline
  final def takeAverage[P <: PieceFunction](t1: Double, t2: Double,
                        z: AlwaysDefinedSpline[P]): Double = {
    z.average(math.min(t1, t2), math.max(t1, t2))
  }

  @inline
  final def iterateFirst(timeStep: Double, pVal1: Double,
                    val2: Double,
                    pVal2: Double, pVal3: Double,
                    pDef: Array[Double],
                    conds: Array[AlwaysDefinedSpline[PieceFunction]],
                    toPassion: Array[Double]): Unit = {
    val a = cons(pVal1, pVal2, pDef(0), conds(0))
    val c = cons(pVal2, pVal3, pDef(1), conds(1))
    val vect = - val2 / timeStep - a * pVal1
    forwardFirst(- (a + c) - 1.0 / timeStep, c, vect, toPassion)
  }

  @inline
  final def iteration(time: Double, firstT: Double, t: Array[Double], lastT: Double,
  predict: Array[Double], preDef: Array[Array[Double]],
  conductivities: Array[Array[AlwaysDefinedSpline[PieceFunction]]],
  toPassion: Array[Array[Double]], result: Array[Double]): Unit = {

    var z = 0
    var t1: Double = firstT
    var t2: Double = predict(z)
    var t3: Double = predict(z + 1)

    var a: Double = cons(t1, t2, preDef(z)(0), conductivities(z)(0))
    var c: Double = cons(t2, t3, preDef(z)(1), conductivities(z)(1))
    var vect = - t(z) / time - a * t1

    forwardFirst(- (a + c) - 1.0 / time, c, vect, toPassion(z))
    z += 1

    while(z < t.length - 1){
      t1 = t2
      t2 = predict(z)
      t3 = predict(z + 1)

      a  = cons(t1, t2, preDef(z)(0), conductivities(z)(0))
      c  = cons(t2, t3, preDef(z)(1), conductivities(z)(1))
      vect = - t(z) / time

      forwardUnit(a, - (a + c) - 1.0 / time, c,  vect,
      toPassion(z - 1)(0), toPassion(z - 1)(1), toPassion(z))
      z += 1
    }

    t1 = t2
    t2 = predict(z)
    t3 = lastT

    a = cons(t1, t2, preDef(z)(0), conductivities(z)(0))
    c = cons(t2, t3, preDef(z)(1), conductivities(z)(1))
    vect = - t(z) / time - c * t3

    forwardLast(a, - (a + c) - 1.0 / time, vect,
    toPassion(z - 1)(0), toPassion(z -1)(1), toPassion(z))

    backwardPassion(toPassion, result)
  }

  final def iteration(time: Double, firstT: Double, t: Array[Double], lastT: Double,
  predict: Array[Double], preDef: Array[Array[Double]],
                      z: AlwaysDefinedSpline[PieceFunction],
  toPassion: Array[Array[Double]], result: Array[Double]): Unit = {

    var r = 0

    var t1 = firstT
    var t2 = predict(r)
    var t3 = predict(r + 1)

    var a: Double = cons(t1, t2, preDef(r)(0), z)
    var c: Double = cons(t2, t3, preDef(r)(1), z)
    var vect: Double = - t(r) / time - a * t1
    forwardFirst(- (a + c) - 1.0 / time, c, vect, toPassion(r))
    r += 1

    while(r < t.length - 1){
      t1 = t2
      t2 = predict(r)
      t3 = predict(r + 1)

      a = cons(t1, t2, preDef(r)(0), z)
      c = cons(t2, t3, preDef(r)(1), z)
      vect = - t(r) / time
      forwardUnit(a, - (a + c) - 1.0 / time, c, vect,
      toPassion(r - 1)(0), toPassion(r - 1)(1), toPassion(r))
      r += 1
    }

    t1 = t2
    t2 = predict(r)
    t3 = lastT

    a = cons(t1, t2, preDef(r)(0), z)
    c = cons(t2, t3, preDef(r)(1), z)
    vect = - t(r) / time - c * t3

    forwardLast(a, - (a + c) - 1/time, vect,
    toPassion(r - 1)(0), toPassion(r - 1)(1), toPassion(r))

    backwardPassion(toPassion, result)
  }

  /** Experimental iteration solver,
    * which can iterate over grid with different lengths with as low level, as seems to be possible,
    *
    * @param time time, sec
    * @param indices sequence of indices of row or column
    * @param lengths length of each matrix row or column
    * @param upper upper bound values array
    * @param grid grid
    * @param predicted predicted grid, have the same size as grid
    * @param localResult empty array with length of longest row or column
    * @param result result grid, have the same size as grid
    * @param lower lower bound array
    * @param preDef predefined tridiagonal matrix values,
    *               which can be calculated by `arraygrid.makeOrthoginalMatrix` and `arraygrid.makeRadialMatrix`
    *               procedures
    * @param conds coductivities. Must have the same size as grid
    * @param toPassion empty massive, which store values, calculated after forward passion.
    *                  Have the same as `localResult` length
    */
  @inline
  final def iteration(time: Double, indices: Array[Int], lengths: Array[Int], upper: Array[Double],
                      grid: Array[Double], predicted: Array[Double], localResult: Array[Double], result: Array[Double],
                      lower: Array[Double], preDef: Array[Array[Double]],
                      conds: Array[Array[AlwaysDefinedSpline[PieceFunction]]],
                      toPassion: Array[Array[Double]]) = {
    var line = 0 // Indexes, passed to upper and lower bounds arrays
    var flatten = 0 // Indexes, passed to preDef array
    while(line != lengths.length) {
      var local = 0
      val localLength = lengths(line)
      while (local != localLength) {
        var global = indices(flatten) // global index passed to one dimensional array, that represents matrix
        var t1: Double = upper(line)
        var t2: Double = predicted(global)
        var t3: Double = predicted(global + 1)

        var a: Double = cons(t1, t2, preDef(flatten)(0), conds(flatten)(0))
        var c: Double = cons(t2, t3, preDef(flatten)(1), conds(flatten)(1))
        var vect = - grid(global) / time - a * t1

        forwardFirst(-(a + c) - 1.0 / time, c, vect, toPassion(local))
        local += 1
        flatten += 1
        global = indices(flatten)


        while(local < localLength - 1) {
          t1 = t2
          t2 = predicted(global)
          t3 = predicted(global + 1)

          a = cons(t1, t2, preDef(flatten)(0), conds(flatten)(0))
          c = cons(t2, t3, preDef(flatten)(1), conds(flatten)(1))
          vect = - grid(global) / time

          forwardUnit(a, -(a + c) - 1.0 / time, c, vect,
            toPassion(local - 1)(0), toPassion(local - 1)(1), toPassion(local))
          local += 1
          flatten += 1
          global = indices(flatten)
        }

        t1 = t2
        t2 = predicted(local)
        t3 = lower(line)

        a = cons(t1, t2, preDef(flatten)(0), conds(flatten)(0))
        c = cons(t2, t3, preDef(flatten)(1), conds(flatten)(1))
        vect = - grid(global) / time - c * t3

        forwardLast(a, -(a + c) - 1.0 / time, vect,
          toPassion(local - 1)(0), toPassion(local - 1)(1), toPassion(local))

        backwardPassion(toPassion, localResult, localLength)
        flatten += 1
        local = 0
        var flatten2 = flatten - localLength
        // write local result to global result
        while(local != localLength){
          result.update(indices(flatten2), localResult(local))
          local += 1
          flatten2 += 1
        }
      }
      line += 1
    }
  }

  @inline
  private final def del(c: Double, b: Double, delta: Double): Double = c + b*delta

  @inline
  private final def lam(r: Double, b: Double, lambda: Double, del: Double): Double = (r - b * lambda) / del


  @inline
  final def forwardFirst(a: Double, c: Double, time: Double, vect: Double, res: Array[Double]): Unit = {
    forwardFirst(- (a + c) - 1.0 / time, c, vect, res)
  }

  @inline
  final def forwardFirst(c: Double, d: Double, vect: Double, res: Array[Double]): Unit = {
    res.update(0, - d / c)
    res.update(1, vect / c)
  }

  @inline
  final def forwardUnit(a: Double, c: Double, time: Double, vect: Double, res: Array[Double]): Unit = {
    forwardFirst(- (a + c) - 1.0 / time, c, vect, res)
  }

  @inline
  final def forwardUnit(b: Double, c: Double, d: Double,
                        vect: Double, delta: Double,
                        lambda: Double, res: Array[Double]): Unit = {
    // Корректность прогонки
    assertion(b, c, d, vect)
    val bigDelta = del(c, b, delta)
    res.update(0, - d / bigDelta)
    res.update(1, lam(vect, b, lambda, bigDelta))
  }


  @inline
  final def forwardLast(b: Double, c: Double,
                        vect: Double, delta: Double,
                        lambda: Double, res: Array[Double]): Unit = {
    val bigDelta = del(c, b, delta)
    val nextLambda = lam(vect, b, lambda, bigDelta)
    res.update(0, 0.0)
    res.update(1, nextLambda)
  }

  @inline
  final def assertion(b : Double, c : Double, d : Double, vect : Double) : Unit ={
    assert(abs(c) >= abs(d) + abs(b), "Passion is not correct or unstable." +
      f" w: $b%2.7f, c: $c%2.7f, d: $d%2.7f. Vector: $vect%2.7f.")
  }

  def backwardPassion(coeffficients: Array[Array[Double]],
                      grid: TwoDGrid.Grid,
                      iter: IterOps
                     ): Unit = {

    /** Расчёт результата */
    @inline def point(vals: Array[Double], prev: Double): Double = {
      val delta = vals(0); val lambda = vals(1)
      delta * prev + lambda
    }
    iter.next
    var inter = 0.0
    while (iter.hasPrev) {
      val i = iter.prev
      inter = point(coeffficients(iter.posAtLayer), inter)
      grid.put(i, inter)
    }
  }

  def backwardRow(coefficients: Array[Array[Double]],
                  grid: Array[Double], row: Int): Unit = {
    val colsNum = coefficients.length
    var col = colsNum - 1
    var g = row * colsNum  + col

    /** Расчёт результата */
    @inline def point(vals: Array[Double], prev: Double): Double = {
      val delta = vals(0); val lambda = vals(1)
      delta * prev + lambda
    }
    var inter = 0.0
    while (col != -1){
      // Расчёт текущего значения (записывается для использования на следующем шаге)
      inter = point(coefficients(col), inter)
      grid.update(g, inter)
      g -= 1
      col -= 1
    }
  }

  def backwardColumn(coefficients: Array[Array[Double]],
                     grid: Array[Double], column: Int, colsNum: Int) = {
    val rowsNum = coefficients.length
    var row = rowsNum - 1
    var g = row * colsNum + column
    /** Расчёт результата */
    @inline def point(vals: Array[Double], prev: Double): Double = {
      val delta = vals(0); val lambda = vals(1)
      delta * prev + lambda
    }
    var inter = 0.0
    while (row != -1){
      // Расчёт текущего значения (записывается для использования на следующем шаге)
      inter = point(coefficients(row), inter)
      grid.update(g, inter)
      g -= colsNum
      row -= 1
    }
  }


  def backwardPassion(coefficients: Array[Array[Double]], result: Array[Double], length: Int): Unit = {


    @inline def point(vals: Array[Double], prev: Double): Double = {
      val delta = vals(0); val lambda = vals(1)
      delta * prev + lambda
    }

    var i = length - 1
    var inter = 0.0
    while (i > -1){
      // Расчёт текущего значения (записывается для использования на следующем шаге)
      inter = point(coefficients(i), inter)
      result.update(i, inter)
      i-=1
    }
  }

  def backwardPassion(coefficients: Array[Array[Double]], result: Array[Double]) : Unit = {
    backwardPassion(coefficients, result, result.length)
  }
}
