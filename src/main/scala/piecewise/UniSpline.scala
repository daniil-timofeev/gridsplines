package piecewise
import com.twitter.algebird._
import piecewise.Spline.PieceFunFactory
import piecewise.intervaltree._

/**
  *
  */
class UniSpline[+S <: PieceFunction](content: NonEmptyITree[Double, S, Upper])
  extends Spline[S](content){

  private val lowerX = lowerBound
  private val upperX = upperBound
  private lazy val lower = apply(lowerX)
  private lazy val upper = apply(upperX)

  override def apply(x: Double): Double = x match{
      case low if low < lowerX => lower
      case upp if upp > upperX => upper
      case cen => super.apply(cen)
    }

  override def applyOption(x: Double): Option[Double] = {
    Some(apply(x))
  }

  override def der(x: Double): Double = x match{
    case low if low < lowerX => 0.0
    case upp if upp > upperX => 0.0
    case cen => super.der(cen)
  }

  override def derOption(x: Double): Option[Double] = {
    Some(der(x))
  }

  override def integral(x: Double): Double = x match{
    case low if low < lowerX => low * x
    case upp if upp > upperX => upp * x
    case cen => super.der(cen)
  }

  override def integralOption(x: Double): Option[Double] = {
    Some(integral(x))
  }

  override def sliceLower(bound: Double): Option[UniSpline[S]] = {
    super.sliceLower(bound).map(_.toUniSpline)
  }

  override def sliceUpper(bound: Double): Option[UniSpline[S]] = {
    super.sliceUpper(bound).map(_.toUniSpline)
  }

  override
  def ++[T >: S <: PieceFunction](spl: Spline[T]): UniSpline[T] = {
    super.++(spl).toUniSpline
  }

  override
  def map[B <: PieceFunction](xy: (Double, Double) => Double)(
                              implicit builder: PieceFunFactory[B]): Option[UniSpline[B]] = {
    UniSpline[B](
      points.map{t =>
        val (x, y) = t
        val newY = xy(x, y)
        (x, newY)
      }.toList)
  }

  override
  def /[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]): Option[UniSpline[B]] = {
    UniSpline(super.sumArguments(spl).map(x => (x, apply(x) / spl(x))))(builder)
  }

  override
  def +[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]
  ): Option[UniSpline[B]] = {
    UniSpline(super.sumArguments(spl).map(x => (x, apply(x) + spl(x))))(builder)
  }

  override
  def -[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]): Option[UniSpline[B]] = {
    UniSpline(super.sumArguments(spl).map(x => (x, apply(x) - spl(x))))(builder)
  }

  override
  def *[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]): Option[UniSpline[B]] = {
    UniSpline(super.sumArguments(spl).map(x => (x, apply(x) * spl(x))))(builder)
  }

  override def convert[R <: PieceFunction](f: SplineConvert[S, R]): UniSpline[R] = {
    content.map((l: Double, u: Double, pf: S) => (l, u, f.apply(l, u, pf))) match {
      case nonEmpty: NonEmptyITree[Double, R, Upper] => new UniSpline[R](nonEmpty)
      case empty: EmptyNode[Double, R] => ???
    }
  }

  override
  def splitNodes(f: (Double, Double, S) => Int): UniSpline[S] = {
    val result =
      content.iterator.flatMap{t =>
        val ((low: Double, upp: Double), s: S) = t
        val size = f(low, upp, s)
        val length = upp - low
        val step = length / size
        val it0 =
          Iterator
            .iterate(low)(low => low + step)
            .takeWhile(value => value <= upp - step)

        val it1 =
          Iterator
            .iterate(low + step)(low => low + step)
            .takeWhile(value => value <= upp)

        val it2 = Iterator.continually(s)
        it0.zip(it1).zip(it2)
      }

    val (it, itt)= result.duplicate

    val size = itt.size

    IntervalTree.buildLeft(it, size) match {
      case nonEmpty: NonEmptyITree[Double, S, Upper] => new UniSpline(nonEmpty)
      case empty: EmptyNode[Double, S] =>
        throw new RuntimeException("Something goes wrong..." +
          " split operation with non empty spline must have non empty result")
    }
  }

  override def average(low: Double, upp: Double): Double = {
    import math._
    if (low > upperX) upper
    else if (upp < lowerX) lower
    else {
      val l = math.max(lowerX, low)
      val u = math.min(upperX, upp)
      super.average(l, u) +
        (max(0.0, upp - upperX) + max(0.0, lowerX - low)) / (upp - low)
    }
  }

}
object UniSpline{

  def apply[S <: PieceFunction](spline: Spline[S]): UniSpline[S] =
    spline.toUniSpline


    def apply[S <: PieceFunction: PieceFunFactory](
              vect: List[(Double, Double)]): Option[UniSpline[S]] = {
      val v = vect.sortBy(_._1)
      val factory = implicitly[PieceFunFactory[S]]
      val pieceFunctions = factory(v)

      if (pieceFunctions.isEmpty) {
        None
      }
      else {
        val initial =
          v.map(_._1)
            .sliding(2)
            .map(list => (list(0), list(1)))
            .zip(pieceFunctions)

        IntervalTree.buildLeft(initial, v.size - 1) match {
          case empty: EmptyNode[Double, S] => None
          case nonEmpty: NonEmptyITree[Double, S, Upper] =>
            Some(new UniSpline(nonEmpty))
        }
      }
    }

}
