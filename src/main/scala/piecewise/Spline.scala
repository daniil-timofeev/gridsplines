package piecewise

import com.twitter.algebird.Interval.{InLowExUp, MaybeEmpty}
import com.twitter.algebird.{Intersection, _}
import piecewise.Spline.{MakePieceFunctions, SlicePieceFunction}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import Spline._
import piecewise.intervaltree.IntervalTree
/**
  * Created by Даниил on 24.01.2017.
  */
 class Spline[S <: PieceFunction](private val content: Option[IntervalTree[Double, S]]){
//TODO add possibility to find interval with some others piece functions types

  def apply(x: Double): Double = {
    IntervalTree.find(x, content).get.v.apply(x)
  }

  def applyOption(x: Double): Option[Double] = {
    IntervalTree.find(x, content).map(_.v.apply(x))
  }

  def der(x: Double): Double = {
    IntervalTree.find(x, content).get.v.derivative(x)
  }

  def derOption(x: Double): Option[Double] = {
    IntervalTree.find(x, content).map(_.v.der(x))
  }

  def integral(x: Double): Double = {
    IntervalTree.find(x, content).get.v.integral(x)
  }

  def integralOption(x: Double): Option[Double] = {
    IntervalTree.find(x, content).map(_.v.integral(x))
  }



  def area(lower: Double, upper: Double): Double = ???

  def areaOption(lower: Double, upper: Double): Option[Double] = ???

}
object Spline{

  def apply[S <: PieceFunction: MakePieceFunctions](vect: List[(Double, Double)]): Spline[S] = {
    val maker = implicitly[MakePieceFunctions[S]]
    val pieceFunctions = maker(vect)
    val initial = (vect, vect drop 1, pieceFunctions).zipped.map{(f, s, pf) =>{
      (Interval.leftClosedRightOpen(f._1, s._1), pf)
    }}.collect{
      case (MaybeEmpty.NotSoEmpty(i: InLowExUp[Double]), func) => (i, func)
    }.toVector
    new Spline[S](IntervalTree.apply(initial))
  }

  def empty = new Spline[PieceFunction](None)

  abstract class MakePieceFunctions[P <: PieceFunction]{

    def apply(args: List[(Double, Double)]): Vector[P]

    def apply(args: (Int) => Double,
              funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[P]

    def apply(argVals: List[Double],
              funcVals: List[Double],
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[P]
  }

  @tailrec
  protected final def makeArgsFromFunc(args: (Int) => Double,
                               interval: Intersection[InclusiveLower, ExclusiveUpper, Double],
                               res: ListBuffer[Double] = ListBuffer.empty[Double],
                               i: Int = 0): List[Double] = {
    i match{
      case in if interval(args(i)) => makeArgsFromFunc(args, interval, res += args(i), i + 1)
      case before if !interval(args(i)) && interval.upper(args(i)) => makeArgsFromFunc(args, interval, res, i + 1)
      case after => res.result()
    }
  }

  @tailrec final def getArgsWithIndexes(args: (Int) => Double,
                                      interval: Intersection[InclusiveLower, ExclusiveUpper, Double],
                                      res: collection.mutable.Builder[(Double, Int), Vector[(Double, Int)]] = Vector.newBuilder[(Double, Int)],
                                      i: Int = 0): Vector[(Double, Int)] = {
    i match{
      case in if interval(args(i)) => getArgsWithIndexes(args, interval, res += Tuple2(args(i), i), i + 1)
      case before if !interval(args(i)) && interval.upper(args(i)) => getArgsWithIndexes(args, interval, res, i + 1)
      case after => res.result()
    }
  }

  implicit object MakeCHermitPieceFunctions extends MakePieceFunctions[Hermit3]{

    override def apply(vect: List[(Double, Double)]): Vector[Hermit3] = {
      Hermit3.apply(vect.toList)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Hermit3] = {
      val argVals = makeArgsFromFunc(args, interval)
      Hermit3(argVals, argVals.map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) = ???
  }

  implicit object MakeCHermitM1PieceFunctions extends MakePieceFunctions[M1Hermit3]{

    override def apply(vect: List[(Double, Double)]): Vector[M1Hermit3] = {
      M1Hermit3.apply(vect.toList)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[M1Hermit3] = {
      val argVals = makeArgsFromFunc(args, interval)
      M1Hermit3(argVals, argVals map (funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[M1Hermit3] = ???
  }

  implicit object MakeLinePieceFunctions extends MakePieceFunctions[Line]{

    override def apply(vect: List[(Double, Double)]): Vector[Line] = {
      Line.apply(vect.toList)
    }


    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Line] = {
      val argVals = makeArgsFromFunc(args, interval)
      Line(argVals, argVals map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Line] = ???
  }

  implicit object MakeCLangrangePieceFunctions extends MakePieceFunctions[Lagrange3]{

    override def apply(vect: List[(Double, Double)]): Vector[Lagrange3] = {
      Lagrange3.apply(vect.toList)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Lagrange3] = {
      val argVals = makeArgsFromFunc(args, interval)
      Lagrange3(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Lagrange3] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) => interval(point._1))
      Lagrange3(atInterval)
    }
  }

  implicit object MakeSquarePieceFunctions extends MakePieceFunctions[Lagrange2]{

    override def apply(vect: List[(Double, Double)]): Vector[Lagrange2] = {
      Lagrange2.apply(vect.toList)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Lagrange2] = {
      val argVals = makeArgsFromFunc(args, interval)
      Lagrange2(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Lagrange2] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) => interval(point._1))
      Lagrange2(atInterval)
    }
  }

  abstract class SlicePieceFunction[S <: PieceFunction]{

    def slicePieceFunctionUpper(pieceFunc: S, to: Double): S

    def slicePieceFunctionLower(pieceFunc: S, from: Double): S

  }

  implicit object SliceHermit3 extends SlicePieceFunction[Hermit3]{

    override def slicePieceFunctionUpper(pieceFunc: Hermit3, to: Double): Hermit3 = {
      val Hermit3(
      yL: Double, yUp: Double,
      dL: Double, dUp: Double,
      interval: InLowExUp[Double]) = pieceFunc
      pieceFunc.copy(yL, pieceFunc(to), dL, pieceFunc.derivative(to), interval.copy(upper = interval.upper.copy(to)))
    }

    override def slicePieceFunctionLower(pieceFunc: Hermit3, from: Double): Hermit3 = {
      val Hermit3(
      yL: Double, yUp: Double,
      dL: Double, dUp: Double,
      interval: InLowExUp[Double]) = pieceFunc
      pieceFunc.copy(pieceFunc(from), yUp, pieceFunc.derivative(from), dUp, interval.copy(lower = interval.lower.copy(from)))
    }
  }

  implicit object SliceM1Hermit3 extends SlicePieceFunction[M1Hermit3]{
    override def slicePieceFunctionUpper(pieceFunc: M1Hermit3, to: Double): M1Hermit3 = {
      val M1Hermit3(
      yL: Double, yUp: Double,
      dL: Double, dUp: Double,
      interval: InLowExUp[Double]) = pieceFunc
      pieceFunc.copy(yL, pieceFunc(to), dL, pieceFunc.derivative(to), interval.copy(upper = interval.upper.copy(to)))
    }

    override def slicePieceFunctionLower(pieceFunc: M1Hermit3, from: Double): M1Hermit3 = {
      val M1Hermit3(
      yL: Double, yUp: Double,
      dL: Double, dUp: Double,
      interval: InLowExUp[Double]) = pieceFunc
      pieceFunc.copy(pieceFunc(from), yUp, pieceFunc.derivative(from), dUp, interval.copy(lower = interval.lower.copy(from)))
    }
  }

  implicit object SliceLagrange3 extends SlicePieceFunction[Lagrange3]{
    override def slicePieceFunctionUpper(pieceFunc: Lagrange3, to: Double): Lagrange3 = {

      pieceFunc.copy(interval = pieceFunc.interval.copy(upper = pieceFunc.interval.upper.copy(to)))
    }

    override def slicePieceFunctionLower(pieceFunc: Lagrange3, from: Double): Lagrange3 = {
      pieceFunc.copy(interval = pieceFunc.interval.copy(lower = pieceFunc.interval.lower.copy(from)))
    }
  }

  implicit object SliceLagrange2 extends SlicePieceFunction[Lagrange2]{
    override def slicePieceFunctionUpper(pieceFunc: Lagrange2, to: Double): Lagrange2 = {
      pieceFunc.copy(interval = pieceFunc.interval.copy(upper = pieceFunc.interval.upper.copy(to)))
    }

    override def slicePieceFunctionLower(pieceFunc: Lagrange2, from: Double): Lagrange2 = {
      pieceFunc.copy(interval = pieceFunc.interval.copy(lower = pieceFunc.interval.lower.copy(from)))
    }
  }

    implicit object SliceLine extends SlicePieceFunction[Line]{
    override def slicePieceFunctionUpper(pieceFunc: Line, to: Double): Line= pieceFunc.sliceUpper(to)


    override def slicePieceFunctionLower(pieceFunc: Line, from: Double): Line = pieceFunc.sliceLower(from)
  }

  implicit object SliceConst extends SlicePieceFunction[Const]{
    override def slicePieceFunctionUpper(pieceFunc: Const, to: Double): Const = {
      pieceFunc.copy(interval = pieceFunc.interval.copy(upper = pieceFunc.interval.upper.copy(to)))
    }

    override def slicePieceFunctionLower(pieceFunc: Const, from: Double): Const = {
      pieceFunc.copy(interval = pieceFunc.interval.copy(lower = pieceFunc.interval.lower.copy(from)))
    }
  }

}
