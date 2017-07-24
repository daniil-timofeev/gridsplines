package piecewise

import com.twitter.algebird.Interval.{InLowExUp, MaybeEmpty}
import com.twitter.algebird.{Intersection, Interval, _}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import Spline._
import com.twitter.algebird.monad.Trampoline.call
import com.twitter.algebird.monad.{Done, Trampoline}
import piecewise.intervaltree.IntervalTree
import piecewise.intervaltree.IntervalTree.{InternalNode, Leaf}

import scala.collection.mutable
/**
  * Created by Даниил on 24.01.2017.
  */
 class Spline[+S <: PieceFunction](protected val content: Option[IntervalTree[Double, S]]){
//TODO add possibility to find interval with some others piece functions types

  def apply(x: Double): Double = {
    IntervalTree.find(x, content).getOrElse(throw new java.util.NoSuchElementException(
      f"Spline apply fails on $x input, with funcs: ${sources.toList.toString}"
    )).v.apply(x)
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

  def area(lower: Double, upper: Double): Double = {
    import scala.collection._
    implicit val iter: mutable.Builder[Double, Iterator[Double]] =
      Iterator.IteratorCanBuildFrom.apply()

    val i = Interval.leftClosedRightOpen(lower, upper)

    content.foreach{it =>
      it.collect{
        case (interval: InLowExUp[Double],
              fun: PieceFunction) if interval.intersect(i) != Empty() => {
          interval.intersect(i) match{
            case Empty() =>  0.0
            case Intersection(InclusiveLower(l), ExclusiveUpper(u)) => {
              fun.area(l, u)
            }
            case Intersection(ExclusiveLower(l), ExclusiveUpper(u)) => {
              fun.area(l, u)
            }
          }
        }
      }
    }
    iter.result().foldLeft(0.0)((x0, x1) => x0 + x1)
  }

  import com.twitter.algebird.monad._
  def sources = Trampoline.run(IntervalTree.toList(
    content,
    ListBuffer.empty[(InLowExUp[Double], S)]
  )).result()

  def points: Iterator[(Double, Double)] = {
    val builder = Iterator.IteratorCanBuildFrom[(Double, Double)].apply()
    Trampoline
      .run(Spline.mutablePoint[S](
        content,
        builder,
        0))
      builder.result()
  }

  def sliceUpper(bound: Double): Spline[S] = {
    new Spline[S](content.map(c => c.sliceUpper(bound)))
  }

  def sliceLower(bound: Double): Spline[S] = {
    new Spline[S](content.map(c => c.sliceLower(bound)))
  }

  def ++ [T >: S <: PieceFunction](spl: Spline[T]): Spline[T] = {
    val mutSources = Trampoline.run(IntervalTree.toList(
      content,
      ListBuffer.empty[(InLowExUp[Double], S)]
    ))
    val src: List[(InLowExUp[Double], T)] = spl.sources
    //TODO make checks if intervals adjascents

    val resSources: ListBuffer[(InLowExUp[Double], T)] =
      mutSources.asInstanceOf[ListBuffer[(InLowExUp[Double], T)]].++=(src)
    new Spline[T](Trampoline.run(
      IntervalTree.buildLeft(resSources.result())
    ))
  }

  def toUniSpline: UniSpline[S] = new UniSpline[S](content)

  def asUniSpline: Spline[PieceFunction] = Spline.makeUniSpline(this)
}
object Spline{

  def makeUniSpline[S <: PieceFunction](spline: Spline[S]): Spline[PieceFunction] = {
    import com.twitter.algebird._
    import com.twitter.algebird.Interval.MaybeEmpty._
    val (lowerX, upperX, lower, upper) = Spline.boundsOf(spline)

    val low = Const(lower)
    val lowSource = Interval.leftClosedRightOpen(Double.MinValue, lowerX) match {
      case NotSoEmpty(interval: InLowExUp[Double]) => Some((interval, low))
      case SoEmpty() => None
    }
    val upp = Const(upper)
    val uppSource = Interval.leftClosedRightOpen(upperX, Double.MaxValue) match {
      case NotSoEmpty(interval: InLowExUp[Double]) => Some((interval, upp))
      case SoEmpty() => None
    }

    val lb: ListBuffer[(InLowExUp[Double], PieceFunction)] =
      Trampoline.run(IntervalTree.toList(
        spline.content,
        ListBuffer.empty[(InLowExUp[Double], S)]
      )).asInstanceOf[ListBuffer[(InLowExUp[Double], PieceFunction)]]

    if (lowSource.nonEmpty) lb.prepend(lowSource.get)
    if (uppSource.nonEmpty) lb.append(uppSource.get)

    new Spline[PieceFunction](Trampoline.run(IntervalTree.buildLeft(lb.result())))
  }

  /** Bound points of spline
    *
    * @param spline spline, from which points is extracted
    * @tparam T type of spline
    * @return (lower x, upper x, lower y, upper y)
    */
  def boundsOf[T <: PieceFunction](spline: Spline[T])
  : (Double, Double, Double, Double) = {
    val p = spline.points
    val (lowX, lowY) = p.next
    var waitForLast: (Double, Double) = (0.0, 0.0)
    while (p.hasNext) {
      waitForLast = p.next()
    }
    val (uppX, uppY) = waitForLast
    (lowX, uppX, lowY, uppY)
  }

  def mutablePoint[V <: PieceFunction](tree: Option[IntervalTree[Double, V]],
                                       buffer: mutable.Builder[(Double, Double),
                                       Iterator[(Double, Double)]],
                                       size: Int)
  : Trampoline[Integer] = {

    def app(interval: InLowExUp[Double], v: V, buffer: mutable.Builder[(Double, Double),
      Iterator[(Double, Double)]], f: Integer)
    : Trampoline[Integer] = {
      var resSize = size
      if(f == 0){
        val low = interval.lower.lower
        resSize += 1
        buffer += ((low, v.apply(low)))
      }
      val upp = interval.upper.upper
      resSize += 1
      buffer += ((upp, v.apply(upp)))
      Done(resSize)
    }

    tree match{
      case None => Done(size)
      case Some(InternalNode(interval, v, None, right)) => {
        for {
          c <- call(app(interval, v, buffer, size))
          r <- call(mutablePoint(right, buffer, c))
        } yield r
      }
      case Some(InternalNode(interval, v, left, right)) => {
        for {
          l <- call(mutablePoint(left, buffer, size))
          c <- call(app(interval, v, buffer, l))
          r <- call(mutablePoint(right, buffer, c))
        } yield r
      }
      case Some(Leaf(interval, v)) => {
        app(interval, v, buffer, size)
      }
    }
  }

  def points[V <: PieceFunction](tree: Option[IntervalTree[Double, V]])
  : Trampoline[List[(Double, Double)]] = {
    tree match{
      case None => Done(List.empty[(Double, Double)])
      case Some(InternalNode(interval, v, left, right)) => {
        for{
          l <- call(points(left))
          r <- call(points(right))
        } yield {
          val low = interval.lower.lower
          if(l.nonEmpty)
            l ::: List[(Double, Double)]((low, v.apply(low))) ::: r
          else{
            val upp = interval.upper.upper
            l ::: List[(Double, Double)]((low, v.apply(low)), (upp, v.apply(upp)))
          }
        }
      }
    }
  }

  def apply[S <: PieceFunction: MakePieceFunctions](vect: List[(Double, Double)]): Spline[S] = {
    val v = vect.sortBy(_._1)
    val maker = implicitly[MakePieceFunctions[S]]
    val pieceFunctions = maker(v)
    val initial = {{v zip {v drop 1}} zip pieceFunctions}
      .collect{
        case((f, s), pf) if f._1 < s._1 =>{
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
      case in if interval(args(i)) =>
        makeArgsFromFunc(args, interval, res += args(i), i + 1)
      case before if !interval(args(i)) && interval.upper(args(i)) =>
        makeArgsFromFunc(args, interval, res, i + 1)
      case after => res.result()
    }
  }

  @tailrec final def getArgsWithIndexes(
                       args: (Int) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double],
                       res: ListBuffer[(Double, Int)] = ListBuffer.empty[(Double, Int)],
                       i: Int = 0
                       ): List[(Double, Int)] = {
    i match{
      case in if interval(args(i)) =>
        getArgsWithIndexes(args, interval, res += Tuple2(args(i), i), i + 1)
      case before if !interval(args(i)) && interval.upper(args(i)) =>
        getArgsWithIndexes(args, interval, res, i + 1)
      case after => res.result()
    }
  }

  implicit object MakeCHermitPieceFunctions extends MakePieceFunctions[Hermite3]{

    override def apply(vect: List[(Double, Double)]): List[Hermite3] = {
      Hermite3.apply(vect)
    }

    override def apply(
                   args: (Int) => Double,
                   funcs: (Double) => Double,
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                      ): List[Hermite3] = {
      val argVals = makeArgsFromFunc(args, interval)
      Hermite3(argVals, argVals.map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(
                   argVals: List[Double],
                   funcVals: List[Double],
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]) = {
      ???
    }
  }

  implicit object MakeCHermitM1PieceFunctions extends MakePieceFunctions[M1Hermite3]{

    override def apply(vect: List[(Double, Double)]): List[M1Hermite3] = {
      M1Hermite3.apply(vect)
    }

    override def apply(args: (Int) => Double,
                       funcs: (Double) => Double,
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
      ): List[M1Hermite3] = {
      val argVals = makeArgsFromFunc(args, interval)
      M1Hermite3(argVals, argVals map (funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double])
    : List[M1Hermite3] = ???
  }

  implicit object MakeLinePieceFunctions extends MakePieceFunctions[Line]{

    override def apply(vect: List[(Double, Double)]): List[Line] = {
      Line.apply(vect)
    }


    override def apply(
                   args: (Int) => Double,
                   funcs: (Double) => Double,
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                   ): List[Line] = {
      val argVals = makeArgsFromFunc(args, interval)
      Line(argVals, argVals map(funcs(_)))
    }

    //TODO implement typeclass apply() method with separated argument a function values
    override def apply(
                   argVals: List[Double],
                   funcVals: List[Double],
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                   ): List[Line] = ???
  }

  implicit object MakeCLangrangePieceFunctions extends MakePieceFunctions[Lagrange3]{

    override def apply(vect: List[(Double, Double)]): List[Lagrange3] = {
      Lagrange3.apply(vect)
    }

    override def apply(
                   args: (Int) => Double,
                   funcs: (Double) => Double,
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                   ): List[Lagrange3] = {
      val argVals = makeArgsFromFunc(args, interval)
      Lagrange3(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(
                   argVals: List[Double],
                   funcVals: List[Double],
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                   ): List[Lagrange3] = {
      val atInterval = (argVals zip funcVals).filter((point: (Double, Double)) =>
        interval(point._1))
      Lagrange3(atInterval)
    }
  }

  implicit object MakeSquarePieceFunctions extends MakePieceFunctions[Lagrange2]{

    override def apply(vect: List[(Double, Double)]): List[Lagrange2] = {
      Lagrange2.apply(vect.toList)
    }

    override def apply(
                   args: (Int) => Double,
                   funcs: (Double) => Double,
                   interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                   ): List[Lagrange2] = {
      val argVals = makeArgsFromFunc(args, interval)
      Lagrange2(argVals.map(x => (x, funcs(x))))
    }

    //TODO implement type class apply() method with separated argument a function values
    override def apply(argVals: List[Double],
                       funcVals: List[Double],
                       interval: Intersection[InclusiveLower, ExclusiveUpper, Double]
                      ): List[Lagrange2] = {
      val atInterval = (argVals zip funcVals)
        .filter((point: (Double, Double)) => interval(point._1))
      Lagrange2(atInterval)
    }
  }

}
