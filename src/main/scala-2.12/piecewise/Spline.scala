package piecewise

import com.twitter.algebird.{Applicative, ExclusiveUpper, InclusiveLower, Intersection}
import piecewise.Spline.MakePieceFunctions

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by Даниил on 24.01.2017.
  */
case class Spline[+S <: PieceFunction : MakePieceFunctions](args: (Int) => Double,
                                       func: (Double) => Double,
                                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) extends Applicative[Spline]{

  val functions = implicitly[MakePieceFunctions[S]].apply(args, func, interval)

  def f(x: Double) = ???

  override def apply[T <: PieceFunction](v: T): Spline[T] = ???

  override def join[T <: PieceFunction, U <: PieceFunction](mt: Spline[T], mu: Spline[U]): Spline[(T, U)] = ???

  override def map[T <: PieceFunction, U <: PieceFunction](m: Spline[T])(fn: (T) => U): Spline[U] = ???

}
object Spline{

  abstract class MakePieceFunctions[P <: PieceFunction]{
    def apply(args: (Int) => Double,
              funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[P]

  }

  @tailrec
  private def makeArgsFromFunc(args: (Int) => Double,
                               interval: Intersection[InclusiveLower, ExclusiveUpper, Double],
                               res: ListBuffer[Double] = ListBuffer.empty[Double],
                               i: Int = 0): List[Double] = {
    i match{
      case in if interval(args(i)) => makeArgsFromFunc(args, interval, res += args(i), i + 1)
      case before if !interval(args(i)) && interval.upper(args(i)) => makeArgsFromFunc(args, interval, res, i + 1)
      case after => res.result()
    }
  }

  implicit object MakeCHermitPieceFunctions extends MakePieceFunctions[CHermit]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[CHermit] = {
      val argVals = makeArgsFromFunc(args, interval)
      CHermit(argVals, argVals.map(funcs(_)))
    }
  }

  implicit object MakeCHermitM1PieceFunctions extends MakePieceFunctions[CHermitM1]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[CHermitM1] = {
      val argVals = makeArgsFromFunc(args, interval)
      CHermitM1(argVals, argVals map (funcs(_)))
    }}

  implicit object MakeLinePieceFunctions extends MakePieceFunctions[CHermitM1]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Line] = {
      val argVals = makeArgsFromFunc(args, interval)
      Line(argVals, argVals map(funcs(_)))
    }}

  implicit object MakeCLangrangePieceFunctions extends MakePieceFunctions[CLagrange]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[CLagrange] = {
      val argVals = makeArgsFromFunc(args, interval)
      CLagrange(argVals.map(x => (x, funcs(x))))
    }
  }

  implicit object MakeSquarePieceFunctions extends MakePieceFunctions[SquarePieceFunc]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[SquarePieceFunc] = {
      val argVals = makeArgsFromFunc(args, interval)
      SquarePieceFunc(argVals.map(x => (x, funcs(x))))
    }
  }
}
