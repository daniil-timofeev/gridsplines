package piecewise

import com.twitter.algebird.{Intersection, _}
import piecewise.Spline.MakePieceFunctions

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by Даниил on 24.01.2017.
  */
case class Spline[+S <: PieceFunction: MakePieceFunctions](
                                functions: Vector[S],
                                interval: Intersection[InclusiveLower, ExclusiveUpper, Double],
                                findInterval: Line) {
//TODO add possibility to find interval with some others piece functions types

  def apply(x: Double): Double = {
    if(interval(x)) functions(findInterval(x))(x)
    else ??? //TODO add decision of method behavior outside of spline interval
  }

  def sliceFrom(value: Double): Spline[S] = {
    val i = PieceFunction.sliceIntervalFrom(value, interval)
    ??? //TODO implement method
  }

  def sliceTo(value: Double): Spline[S] = {
    val i = PieceFunction.sliceIntervalTo(value, interval)
    ??? //TODO implement method
  }

  def slice(from: Double, to: Double): Spline[S] = {
    val i = PieceFunction.sliceIntervalTo(to, PieceFunction.sliceIntervalFrom(from, interval))
    ??? //TODO implement method
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
    val mapper = a.zipWithIndex
    val argFunc =
    Line(
      List(mapper.head, mapper.last)
        .map(t => (t._1, t._2.toDouble))
    ).head
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

  implicit object MakeCHermitPieceFunctions extends MakePieceFunctions[CHermit]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[CHermit] = {
      val argVals = makeArgsFromFunc(args, interval)
      CHermit(argVals, argVals.map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) = ???
  }

  implicit object MakeCHermitM1PieceFunctions extends MakePieceFunctions[CHermitM1]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[CHermitM1] = {
      val argVals = makeArgsFromFunc(args, interval)
      CHermitM1(argVals, argVals map (funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[CHermitM1] = ???
  }

  implicit object MakeLinePieceFunctions extends MakePieceFunctions[CHermitM1]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[Line] = {
      val argVals = makeArgsFromFunc(args, interval)
      Line(argVals, argVals map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[CHermitM1] = ???
  }

  implicit object MakeCLangrangePieceFunctions extends MakePieceFunctions[CLagrange]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[CLagrange] = {
      val argVals = makeArgsFromFunc(args, interval)
      CLagrange(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[CLagrange] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) => interval(point._1))
      CLagrange(atInterval)
    }
  }

  implicit object MakeSquarePieceFunctions extends MakePieceFunctions[SquarePieceFunc]{
    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[SquarePieceFunc] = {
      val argVals = makeArgsFromFunc(args, interval)
      SquarePieceFunc(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): Vector[SquarePieceFunc] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) => interval(point._1))

      SquarePieceFunc(atInterval)
    }
  }
}
