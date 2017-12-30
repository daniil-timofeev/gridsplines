package piecewise

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}

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

  val listGen: Gen[List[(Double, Double)]] = for{
    g <- listOfN[(Double, Double)](4, pointGen)
  } yield g

  property(" Совпадение в точках / Coincidence at consctruction points") =
    forAllNoShrink(listGen.suchThat(_.size > 3))((vals : List[(Double, Double)]) =>{
      vals match {
        case (a, b) :: (c, d) :: (e, f) :: tail if(
          b.equals(d) || d.equals(f) || (a.equals(c) && a.equals(e)))
        => throws(classOf[IllegalArgumentException])(Spline[Hermite3](vals))
        case values =>
          val spline = Spline[Hermite3](values).get
          values.sortBy(_._1).dropRight(1) //Because last point exclusive
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
          val sources = Hermite3.makeSources(values)
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


  property(" Производные вещественны / derivatives is not NaN") =
    forAll(listGen suchThat(_.size > 3)){ (vals : List[(Double, Double)]) =>{
      val spline = Spline[M1Hermite3](vals).get
      vals.sortBy(_._1).dropRight(1) //because last point exclusive
      .map(v => !spline.der(v._1).isNaN)
      .reduce(_ && _)
  }
}


  @inline def error(a : Double, b : Double) : Double = (a, b) match{
    case (0.0, anyB) => 100.0
    case (anyA, 0.0) => 100.0
    case (anyA, anyB) => abs(anyA - anyB) / anyA * 100
  }

}
