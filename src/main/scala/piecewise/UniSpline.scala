package piecewise
import intervaltree._
import com.twitter.algebird
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.Interval.MaybeEmpty.NotSoEmpty
import com.twitter.algebird.monad.Trampoline

import scala.collection.mutable.ListBuffer
/**
  * Created by Даниил on 12.07.2017.
  */
class UniSpline[+S <: PieceFunction](content: Option[IntervalTree[Double, S]]) extends
  Spline[S](content){

  private val (lowerX, upperX, lower, upper) = Spline.boundsOf(this)

  private val partApply =
    PartialFunction[Double, Double]((x: Double) => x match{
      case low if low <= lowerX => lower
      case upp if upp >= upperX => upper
    })

  private val partDer =
    PartialFunction((x: Double) => x match{
      case low if low <= lowerX => 0.0
      case upp if upp >= upperX => 0.0
    })

  private val partIntegral =
    PartialFunction((x: Double) => x match{
      case low if low <= lowerX => low * x
      case upp if upp >= upperX => upp * x
  })

  override def apply(x: Double): Double = {
    partApply.applyOrElse(x, (x: Double) => super.apply(x))
  }

  override def applyOption(x: Double): Option[Double] = {
    Some(apply(x))
  }

  override def der(x: Double): Double = {
    partDer.applyOrElse(x, (x: Double) => super.der(x))
  }

  override def derOption(x: Double): Option[Double] = {
    Some(der(x))
  }

  override def integral(x: Double): Double = {
    partIntegral.applyOrElse(x, (x: Double) => super.integral(x))
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


}
object UniSpline{

  def apply[S <: PieceFunction](spline: Spline[S]): UniSpline[S] =
    spline.toUniSpline


  def asSpline[S <: PieceFunction](spline: Spline[S]): Spline[PieceFunction] = {
    Spline.makeUniSpline(spline)
  }
}
