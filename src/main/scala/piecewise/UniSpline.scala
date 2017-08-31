package piecewise
import intervaltree._
import com.twitter.algebird
import com.twitter.algebird.{ExclusiveUpper, InclusiveLower, Intersection}
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.Interval.MaybeEmpty.NotSoEmpty
import com.twitter.algebird.monad.Trampoline
import piecewise.Spline.MakePieceFunctions

import scala.collection.mutable.ListBuffer
/**
  * Created by Даниил on 12.07.2017.
  */
class UniSpline[+S <: PieceFunction](content: Option[IntervalTree[Double, S]]) extends
  Spline[S](content){

  private val (lowerX, upperX, lower, upper) = Spline.boundsOf(this)

  override def apply(x: Double): Double = x match{
      case low if low <= lowerX => lower
      case upp if upp >= upperX => upper
      case cen => super.apply(cen)
    }

  override def applyOption(x: Double): Option[Double] = {
    Some(apply(x))
  }

  override def der(x: Double): Double = x match{
    case low if low <= lowerX => 0.0
    case upp if upp >= upperX => 0.0
    case cen => super.der(cen)
  }

  override def derOption(x: Double): Option[Double] = {
    Some(der(x))
  }

  override def integral(x: Double): Double = x match{
    case low if low <= lowerX => low * x
    case upp if upp >= upperX => upp * x
    case cen => super.der(cen)
  }

  override def integralOption(x: Double): Option[Double] = {
    Some(integral(x))
  }

  override def sliceLower(bound: Double): UniSpline[S] = {
    super.sliceLower(bound).toUniSpline
  }

  override def sliceUpper(bound: Double): UniSpline[S] = {
    super.sliceUpper(bound).toUniSpline
  }

  override
  def ++[T >: S <: PieceFunction](spl: Spline[T]): UniSpline[T] = {
    super.++(spl).toUniSpline
  }

  override
  def map[B <: PieceFunction](xy: (Double, Double) => Double)(
                              implicit builder: MakePieceFunctions[B]): UniSpline[B] = {
    UniSpline[B](
      points.map{t =>
        val (x, y) = t
        val newY = xy(x, y)
        (x, newY)
      }.toList)
  }

  override
  def /[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: MakePieceFunctions[B]): UniSpline[B] = {
    map[B]((x: Double, y: Double) => y / spl(x))
  }

  override
  def +[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: MakePieceFunctions[B]
  ): UniSpline[B] = {
    map[B]((x: Double, y: Double) => y + spl(x))
  }

  override
  def -[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: MakePieceFunctions[B]): UniSpline[B] = {
    map[B]((x: Double, y: Double) => y - spl(x))
  }

  override
  def *[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: MakePieceFunctions[B]): UniSpline[B] = {
    map[B]((x: Double, y: Double) => y * spl(x))
  }

  override def convert[R <: PieceFunction](f: SplineConvert[S, R]): UniSpline[R] = {
    new UniSpline[R](content.map(_.map(f)))
  }

  override
  def splitWhere(f: (Double, Double, S) => Int): UniSpline[S] = {
    import com.twitter.algebird.field._
    val newTree = content.get.splitWhere(f)
    new UniSpline(newTree)
  }

}
object UniSpline{

  def apply[S <: PieceFunction](spline: Spline[S]): UniSpline[S] =
    spline.toUniSpline


    def apply[S <: PieceFunction: MakePieceFunctions](
              vect: List[(Double, Double)]): UniSpline[S] = {
      val v = vect.sortBy(_._1)
      val maker = implicitly[MakePieceFunctions[S]]
      val pieceFunctions = maker(v)
      val initial = v.sliding(2).zip(pieceFunctions)
        .collect{
          case(Seq(f, s), pf) if f._1 < s._1 =>{
            (Intersection.apply(InclusiveLower(f._1), ExclusiveUpper(s._1)), pf)
          }}
      new UniSpline[S](IntervalTree.apply(initial.toList))
    }

  def asSpline[S <: PieceFunction](spline: Spline[S]): Spline[PieceFunction] = {
    Spline.makeUniSpline(spline)
  }

}
