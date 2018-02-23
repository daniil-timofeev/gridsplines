import com.twitter.algebird.Monoid

import scala.collection.Iterator

/**
  *
  */
package object piecewise {

  @inline
  final def cubicRoots(a: Double, b: Double, c: Double, d: Double): Seq[Double] = {
    import scala.math.pow
    val p = pow(b, 2.0) - 3 * a * c
    val q = 9 * a * b * c - 2 * pow(b, 3.0) - 27.0 * pow(a, 3.0) * d
    val n = 27.0 * pow(p, 3.0) / pow(q, 2.0)
    if (n > 6.75) {
      val theta = 1.0 / math.tan(math.sqrt(4 * n / 27.0 - 1.0))
      val cosin = (x: Double) => p * (4 * pow(math.cos(theta / 3.0 + x), 2.0) - 3.0)
      val x1 = (-b + q / cosin(0)) / (3.0 * a)
      val x2 = (-b + q / cosin(120 / scala.math.Pi)) / (3.0 * a)
      val x3 = (-b + q / cosin(240 / scala.math.Pi)) / (3.0 * a)
      Seq(x1, x2, x3)
    }
    else {
      val root = math.sqrt(pow(q, 2.0) / 4.0 - pow(p, 3.0))
      val n = math.cbrt(q / 2.0 + root) + math.cbrt(q / 2.0 - root)
      Seq((-b + n) / (3.0 * a))
    }

  }


  @inline
  final def cubicRootsVieta(a: Double, b: Double, c: Double): Seq[Double] = {
    import scala.math._
    val q = (math.pow(a, 2.0) - 3.0 * b) / 9.0
    val r = (2 * pow(a, 3.0) - 9.0 * a * b + 27.0 * c) / 54.0
    val m = pow(r, 2.0) - pow(q, 3.0)

    if (m < 0.0) {
      val fi = acos(r / sqrt(pow(q, 3.0)))
      val x1 = -2.0 * sqrt(q) * cos(fi / 3.0) - a / 3.0
      val x2 = -2.0 * sqrt(q) * cos((fi + 2.0 * math.Pi) / 3.0) - a / 3.0
      val x3 = -2.0 * sqrt(q) * cos((fi - 2.0 * math.Pi) / 3.0) - a / 3.0
      Seq(x2, x1, x3)
    }
    else {
      val s = cbrt(-r + sqrt(m))
      val t = cbrt(-r - sqrt(m))
      Seq(s + t - a / 3.0)
    }
  }

  implicit class Itr[A](val iter: Iterator[A]){
    def process[B](fPad: A, first: (A, A, A) => B)
                  (casual: (A, A, A) => B)
                  (last: (A, A, A) => B, lPad: A)(implicit M: Monoid[A]): Iterator[B] = {
      var isFirst = true
      var p0: A = M.zero
      var p1: A = M.zero
      new Iterator[B] {
        override def hasNext: Boolean = iter.hasNext
        override def next(): B = {
          if (isFirst && iter.hasNext){
            val f0 = iter.next()
            if (iter.hasNext) {
              val f1 = iter.next()
              isFirst = false
              p0 = f0
              p1 = f1
              first(fPad, f0, f1)
            }
            else casual(fPad, f0, lPad)
          }
          else if (iter.hasNext) {
            val p2 = iter.next()
            val r = casual(p0, p1, p2)
            p0 = p1
            p1 = p2
            r
          }
          else {
            last(p0, p1, lPad)
          }
        }
      }
    }
  }

  def deriv(xy1: (Double, Double), xy2: (Double, Double)) =
    (xy2._2 - xy1._2) / (xy2._1 - xy1._1)

  def centralDeriv(xy1: (Double, Double),
                   xy12: (Double, Double),
                   xy2: (Double, Double)) = {
    deriv(xy1, xy2)
  }

  def boundDervs(values: List[(Double, Double)]) = {
    val rightVals = values takeRight 2
    val der1 = deriv(values.head, values.tail.head)
    val der2 = deriv(rightVals.head, rightVals.tail.head)
    (der1, der2)
  }

  def derivatives(values: Iterator[(Double, Double)]): Iterator[Double] = {
    values.process((0.0, 0.0),
      (xy0: (Double, Double), xy1: (Double, Double), xy2: (Double, Double)) => {
        deriv(xy1, xy2)
      }
    )(
      (xy0: (Double, Double), xy1: (Double, Double), xy2: (Double, Double)) => {
        centralDeriv(xy0, xy1, xy2)
      }
    )(
      (xy0: (Double, Double), xy1: (Double, Double), xy2: (Double, Double)) => {
        deriv(xy0, xy1)
      }, (0.0, 0.0)
    )
  }

  def derivatives(values: List[(Double, Double)]): Iterator[Double] = {
    val onBound = boundDervs(values)
    onBound._1 :: (values, values drop 2).zipped.map(deriv) :::
      onBound._2 :: Nil
    Iterator(onBound._1) ++
      values.sliding(3).map(list => deriv(list(0), list(2))) ++
      Iterator(onBound._2)
  }

  def h(low: Double, upp: Double) = upp - low

  def delta(yLow: Double, yUpp: Double, low: Double, upp: Double) = {
    (yUpp - yLow) / h(low, upp)
  }

}
