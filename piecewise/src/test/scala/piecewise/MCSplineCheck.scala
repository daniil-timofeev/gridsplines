package piecewise

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}
import piecewise.Hermite3._

import scala.math._


/**
  * @author Daniil
  */
object MCSplineCheck extends Properties("Monotonic spline .."){

  val pointGen = for {
    from <- choose(-100.0, 100.0)
    end <- choose(from, 100.0)
    x <- choose(from, end)
    y <- choose(0.0, 100.0)
  } yield (x, y)

  val listGen: Gen[List[(Double, Double)]] = {
    for{
      g <- listOf[(Double, Double)](pointGen)
    } yield g
  } suchThat(list => list.size > 3)

  property(" Coincidence at consctruction points") =
    forAllNoShrink(listGen)((vals: List[(Double, Double)]) =>{
      vals.sortBy(_._1) match {
        case (a, b) :: (c, d) :: (e, f) :: tail if(
          b.equals(d) || d.equals(f) || (a.equals(c) && a.equals(e)))
        => throws(classOf[IllegalArgumentException])(Spline[Hermite3](vals))
        case values =>
          val spline = Spline[Hermite3](values).get
          values //Because last point exclusive
            .map{point =>
            val res = error(point._2, spline(point._1))
            res < 2.0
          }.reduce(_ && _)
      }}
    )

  property("Analytical monotonicity") =
    forAllNoShrink(listGen suchThat(_.size > 3)){(vals: List[(Double, Double)]) =>
      vals match {
        case (a, b) :: (c, d) :: (e, f) :: tail if (b == d) || d == f || ((a == c) && a == e)
        => throws(classOf[IllegalArgumentException])(Spline[M1Hermite3](vals))
        case values => {
          val sources = Hermite3.makeSources(values.iterator)
                                .map(Hermite3.monothone(_)(Hermite3.Normal))
         true
        }
      }
    }
  property("monotonicity") =
    forAllNoShrink(listGen.suchThat(_.size > 3))((vals : List[(Double, Double)]) => {
      vals match {
        case (a, b) :: (c, d) :: (e, f) :: tail if( b == d) || d == f || ((a == c) && a == e)
        => throws(classOf[IllegalArgumentException])(Spline[M1Hermite3](vals))
        case values =>{
          val spline = Spline[M1Hermite3](values).get
          val v = values sortBy(_._1) dropRight 1 //because last point exclusive
          (v, v drop 1).zipped
          .map {(p0, p1) =>
            val (x0, y0) = p0
            val (x1, y1) = p1
            val y = spline((x0 + x1) / 2.0)
            math.signum(y1 - y) == math.signum(y - y0)
          }
          .reduce(_ && _)
    }}})


  property("derivatives is not NaN") =
    forAllNoShrink(listGen suchThat(_.size > 3)){ (vals : List[(Double, Double)]) => {
      val spline = Spline[M1Hermite3](vals).get
      vals.sortBy(_._1).dropRight(1) //because last point exclusive
      .map(v => !spline.der(v._1).isNaN)
      .reduce(_ && _)
  }
}
  def makeSources(values: List[(Double, Double)]): Iterator[Array[Double]] = {
    values match {
      case Nil => Iterator.empty
      case any :: Nil => Iterator.empty
      case v1 :: v2 :: Nil => {
        Iterator(array(v1._2, v2._2, 0.0, 0.0, v1._1, v2._1))
      }
      case v1 :: v2 :: v3 :: Nil => {
        val der1 = deriv(v1, v2)
        val der2 = deriv(v2, v3)
        val der = deriv(v1, v3)
        Iterator(
          array(v1._2, v2._2, der1, der, v1._1, v2._1),
          array(v2._2, v3._2, der, der2, v2._1, v3._1)
        )
      }
      case vals => {
        val dervs = derivatives(vals)
        (vals.sliding(2) zip dervs.sliding(2)).map(lists => {
          val (((x1, y1) :: (x2, y2) :: Nil), Seq(d1, d2)) = lists
          array(y1, y2, d1, d2, x1, x2)
        })
      }
    }
  }


  property("`makeSources` with Iterator argument implementation" +
           " works the same as with List") =
    forAllNoShrink(listGen suchThat(_.size > 3)){ (vals: List[(Double, Double)]) =>
      val verifiedSum = makeSources(vals).toList
      val iteratorSum = Hermite3.makeSources(vals.iterator).toList

      iteratorSum.map(_.sum).toList.sum ?= verifiedSum.map(_.sum).toList.sum
    }


  @inline def error(a : Double, b : Double) : Double = (a, b) match{
    case (0.0, anyB) => 100.0
    case (anyA, 0.0) => 100.0
    case (anyA, anyB) => abs(anyA - anyB) / anyA * 100
  }

}
