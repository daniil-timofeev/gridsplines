package piecewise

import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.{Intersection, _}
import piecewise.Spline.{MakePieceFunctions, SlicePieceFunction}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import Spline._
/**
  * Created by Даниил on 24.01.2017.
  */
case class Spline[S <: PieceFunction](
                                functions: Vector[S],
                                interval: InLowExUp[Double],
                                findInterval: Line) {
//TODO add possibility to find interval with some others piece functions types

  def apply(x: Double): Double = {
    if(interval(x)) functions(math.round(findInterval(x)).toInt)(x)
    else ??? //TODO add decision of method behavior outside of spline interval
  }

  def sliceUpper[R](to: Double)(implicit slicer: SlicePieceFunction[S]): Spline[S] = {
    slicer.sliceUpper(this, to)
  }

  def sliceLower(from: Double)(implicit slicer: SlicePieceFunction[S]): Spline[S] = {
    slicer.sliceLower(this, from)
  }

  def slice(from: Double, to: Double)(implicit slicer: SlicePieceFunction[S]): Spline[S] = {
   slicer.sliceUpper(slicer.sliceLower(this, from), to)
  }

}
object Spline{

  def apply[S <: PieceFunction: MakePieceFunctions](
    args: Line,
    func: (Double) => Double,
    interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Spline[S] = {
    val maker = implicitly[MakePieceFunctions[S]]
    val argVals = getArgsWithIndexes((i: Int) => args(i), interval)
    val a = argVals.view.map(x => x._1)
    val f = a.map(func(_))
    val functions = maker(a.toList, f.toList, interval)
    val mapper = a.zipWithIndex.toIndexedSeq
    val firstX = (mapper(0)._1 + mapper(1)._1) / 2.0
    val firstY = 0
    val secondX = (mapper(0)._2 + mapper(1)._2) / 2.0
    val secondY = mapper.length - 2
    val argFunc =
      Line((firstX, firstY.toDouble) :: (secondX, secondY.toDouble) :: Nil).head
    new Spline[S](functions, interval, argFunc)
  }

  abstract class MakePieceFunctions[P <: PieceFunction]{
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

    protected def dropUpper(spline: Spline[S], to: Double): Vector[S] = {
      import com.twitter.algebird.Intersection._
      import com.twitter.algebird.Interval._
      spline.functions.takeWhile(f => {
          f.interval.intersect(ExclusiveUpper(to)) match{
            case empty: Empty[_] => true
            case _ => false
          }
        })
    }

    protected def dropLower(spline: Spline[S], from: Double): Vector[S] = {
      spline.functions.dropWhile(f => {
        f.interval.intersect(InclusiveLower(from)) match{
          case empty: Empty[_] => true
          case _ => false
        }
      })
    }

    def sliceUpper(spline: Spline[S], to: Double): Spline[S] = {
      val functions =
      dropUpper(spline, to).collect(
        {
          case f if f.interval.contains(to) => slicePieceFunctionUpper(f, to)
          case default => default
        }
      )
      spline.copy[S](
        functions,
        Intersection(spline.interval.lower, spline.interval.upper.copy(to)),
        spline.findInterval.sliceUpper(to))
    }

    def sliceLower(spline: Spline[S], from: Double): Spline[S] = {
      val functions =
        dropLower(spline, from).collect(
          {
            case f if f.interval.contains(from) => slicePieceFunctionLower(f, from)
            case default => default
          }
        )
      spline.copy[S](
        functions,
        Intersection(spline.interval.lower.copy(from), spline.interval.upper),
        spline.findInterval.sliceLower(from))
    }

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
