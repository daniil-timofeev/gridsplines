package piecewise

import com.twitter.algebird.Interval.{InLowExUp, MaybeEmpty}
import com.twitter.algebird.{Intersection, _}

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

  import com.twitter.algebird.monad._
  def sources = Trampoline.run(IntervalTree.toList(content))

}
object Spline{

  def apply[S <: PieceFunction: MakePieceFunctions](vect: List[(Double, Double)]): Spline[S] = {
    val v = vect.sortBy(_._1)
    val maker = implicitly[MakePieceFunctions[S]]
    val pieceFunctions = maker(v)
    val initial = {{v zip {v drop 1}} zip pieceFunctions}
      .collect{
        case((f, s), pf) if (f._1 < s._1) =>{
      (Intersection.apply(InclusiveLower(f._1), ExclusiveUpper(s._1)), pf)
    }}
    new Spline[S](IntervalTree.apply(initial))
  }

  def empty = new Spline[PieceFunction](None)

  abstract class MakePieceFunctions[P <: PieceFunction]{

    def apply(args: List[(Double, Double)]): List[P]

    def apply(args: (Int) => Double,
              funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[P]

    def apply(argVals: List[Double],
              funcVals: List[Double],
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[P]
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
                                      res: ListBuffer[(Double, Int)] = ListBuffer.empty[(Double, Int)],
                                      i: Int = 0): List[(Double, Int)] = {
    i match{
      case in if interval(args(i)) => getArgsWithIndexes(args, interval, res += Tuple2(args(i), i), i + 1)
      case before if !interval(args(i)) && interval.upper(args(i)) => getArgsWithIndexes(args, interval, res, i + 1)
      case after => res.result()
    }
  }

  implicit object MakeCHermitPieceFunctions extends MakePieceFunctions[Hermite3]{

    override def apply(vect: List[(Double, Double)]): List[Hermite3] = {
      Hermite3.apply(vect)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
              interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Hermite3] = {
      val argVals = makeArgsFromFunc(args, interval)
      Hermite3(argVals, argVals.map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) = ???
  }

  implicit object MakeCHermitM1PieceFunctions extends MakePieceFunctions[M1Hermite3]{

    override def apply(vect: List[(Double, Double)]): List[M1Hermite3] = {
      M1Hermite3.apply(vect)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[M1Hermite3] = {
      val argVals = makeArgsFromFunc(args, interval)
      M1Hermite3(argVals, argVals map (funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[M1Hermite3] = ???
  }

  implicit object MakeLinePieceFunctions extends MakePieceFunctions[Line]{

    override def apply(vect: List[(Double, Double)]): List[Line] = {
      Line.apply(vect)
    }


    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Line] = {
      val argVals = makeArgsFromFunc(args, interval)
      Line(argVals, argVals map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Line] = ???
  }

  implicit object MakeCLangrangePieceFunctions extends MakePieceFunctions[Lagrange3]{

    override def apply(vect: List[(Double, Double)]): List[Lagrange3] = {
      Lagrange3.apply(vect)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Lagrange3] = {
      val argVals = makeArgsFromFunc(args, interval)
      Lagrange3(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Lagrange3] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) => interval(point._1))
      Lagrange3(atInterval)
    }
  }

  implicit object MakeSquarePieceFunctions extends MakePieceFunctions[Lagrange2]{

    override def apply(vect: List[(Double, Double)]): List[Lagrange2] = {
      Lagrange2.apply(vect.toList)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Lagrange2] = {
      val argVals = makeArgsFromFunc(args, interval)
      Lagrange2(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]): List[Lagrange2] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) => interval(point._1))
      Lagrange2(atInterval)
    }
  }

}
