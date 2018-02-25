package piecewise

import com.twitter.algebird._
import piecewise.Spline._
import piecewise.intervaltree._

import scala.collection.immutable.SortedSet

/** Spline with type `S`
  */
 class Spline[+S <: PieceFunction](
         protected val content: NonEmptyITree[Double, S, Upper]){
//TODO add possibility to find interval with some others piece functions types

  @inline
  private def get[S1 >: S <: PieceFunction]( nonEmpty: NonEmptyITree[Double, S1, Upper],
                                             x: Double,
                                             fun: S1 => Double => Double): Double = {
    nonEmpty.v.apply(x)
  }

  /** Value of function at `x`.
    *
    * NOTE: provide unsafe access to value.
    * In a case, where `x` out of the spline interval, an exception will be thrown.
    *
    * @param x function argument
    * @return function value
    */
  def apply(x: Double): Double = {
    IntervalTree.find(x, content) match {
      case empty: EmptyNode[Double, S] => throw new NoSuchElementException(
        f"Spline apply fails on $x input, with funcs: ${sources.toString}")
      case nonEmpty: NonEmptyITree[Double, S, Upper] => get(nonEmpty, x, _.apply)
    }
  }

  /** Value of function at `x`.
    *
    * NOTE: provide safe access to value.
    * Returns `None` in a case, where `x` out of the spline interval.
    *
    * @param x function argument
    * @return function value
    */
  def applyOption(x: Double): Option[Double] = {
    IntervalTree.find(x, content) match {
      case empty: EmptyNode[Double, S] => None
      case nonEmpty: NonEmptyITree[Double, S, Upper] => Some(get(nonEmpty, x, _.apply))
    }
  }


  /** Derivative of function at `x`.
    *
    * NOTE: in a case, when `x` out of the spline interval, the result will be an Exception.
    *
    * @param x function argument
    * @return function derivative
    */
  def der(x: Double): Double = {
    IntervalTree.find(x, content) match {
      case empty: EmptyNode[Double, S] => throw new NoSuchElementException(
        f"Spline der fails on $x input, with funcs: ${sources.toString}")
      case nonEmpty: NonEmptyITree[Double, S, Upper] => get(nonEmpty, x, _.der)
    }
  }

  /** Derivative of function at `x`.
    *
    * NOTE: in case, when `x` out of the spline interval, the result will be None.
    *
    * @param x function argument
    * @return function derivative
    */
  def derOption(x: Double): Option[Double] = {
    IntervalTree.find(x, content) match {
      case empty: EmptyNode[Double, S] => None
      case nonEmpty: NonEmptyITree[Double, S, Upper] => Some(get(nonEmpty, x, _.der))
    }
  }

  /** Antiderivative of function at `x`.
    *
    * NOTE: in case, when `x` out of the spline interval, the result will be None.
    *
    * @param x function argument
    * @return function antiderivative
    */
  def integral(x: Double): Double = {
    IntervalTree.find(x, content) match {
      case empty: EmptyNode[Double, S] =>  throw new NoSuchElementException(
        f"Spline integral fails on $x input, with funcs: ${sources.toList.toString}")
      case nonEmpty: NonEmptyITree[Double, S, Upper] => get(nonEmpty, x, _.integral)
    }
  }
  /** Antiderivative of function at `x`.
    *
    * NOTE: in case, when `x` out of the spline interval, the result will be None.
    *
    * @param x function argument
    * @return function antiderivative
    */
  def integralOption(x: Double): Option[Double] = {
    IntervalTree.find(x, content) match {
      case empty: EmptyNode[Double, S] => None
      case nonEmpty: NonEmptyITree[Double, S, Upper] =>
        Some(get(nonEmpty, x, _.integral))
    }
  }

  def swap = ???

  /** Average value of spline at [`lower` to `upper`] to domain
    *
    * @param lower lower bound
    * @param upper upper bound
    * @return
    */
  def average(lower: Double, upper: Double): Double = {
    import com.twitter.algebird.Monoid._
    IntervalTree.subIntervalFold(
      content, lower, upper,
      (l: Double, u: Double, fun: S) => fun.area(l, u)
    ) / (upper - lower)
  }

  /** Area under the spline domain at [`lower` to `upper`]
    *
    * @param lower lower bound
    * @param upper upper bound
    * @return area under the spline domain
    */
  def area(lower: Double, upper: Double): Double = {
    IntervalTree.subIntervalFold(
      content, lower, upper,
      (l: Double, u: Double, fun: S) => fun.area(l, u)
    )
  }


  def map[R <: PieceFunction](f: S => R): Spline[R] = {

    content.map[Double, S, R]((low: Double, upp: Double, pf: S) =>
      (low, upp, f(pf))) match {
      case nonEmpty: NonEmptyITree[Double, R, Upper] => new Spline(nonEmpty)
      case empty: EmptyNode[Double, R] => ???
    }
  }

  /** Spline sources with ((low, upp], func) format
    */
  def sources: List[((Double, Double), S)] = content.array.toList

  /** Spline sources iterator with ((low, upp], func) format
    *
    */
  def iterator: Iterator[((Double, Double), S)] = {
    content.iterator
  }

  /** Spline building points iterator with (x, f(x)) format
    */
  def points: Iterator[(Double, Double)] = {
    val it = content.iterator
    val builder = Iterator.IteratorCanBuildFrom[(Double, Double)].newIterator
    while (it.hasNext) {
      val point = it.next()
      if (it.hasNext) builder.+=((point._1._1, apply(point._1._1)))
      else {
        builder.+=((point._1._1, apply(point._1._1)))
        builder.+=((point._1._2, apply(point._1._2)))
      }
    }
    builder.result()
  }

  /** Slice spline with new upper bound
    *
    * @param bound new upper bound
    * @return sliced spline
    */
  def sliceUpper(bound: Double): Option[Spline[S]] = {
    import com.twitter.algebird._
    implicit val s = Successible.fromNextOrd[Double](d => Some(d + 1.0))
    content.sliceUpper(bound) match {
      case empty: EmptyNode[Double, S] => None
      case nonEmpty: NonEmptyITree[Double, S, Upper] => Some(new Spline(nonEmpty))
    }
  }

  /** Slice spline with new lower bound
    *
    * @param bound new lower bound
    * @return sliced spline
    */
  def sliceLower(bound: Double): Option[Spline[S]] = {
    import com.twitter.algebird._
    implicit val s = Successible.fromNextOrd[Double](d => Some(d + 1.0))
    content.sliceLower(bound) match {
      case empty: EmptyNode[Double, S] => None
      case nonEmpty: NonEmptyITree[Double, S, Upper] => Some(new Spline(nonEmpty))
    }
  }

  /** Number of spline piece functions */
  def size: Int = content.size

  def ++[T >: S <: PieceFunction](spl: Spline[T]): Spline[T] = {
    val thisPieces = iterator
    val thisSize = size
    val thatPieces = spl.iterator
    val thatSize = spl.size
    val wholePieces: Iterator[((Double, Double), T)] = thisPieces ++ thatPieces
    val wholeSize = thisSize + thatSize
    IntervalTree.buildLeft(wholePieces, wholeSize) match {
      case empty: EmptyNode[Double, T] => ???
      case nonEmpty: NonEmptyITree[Double, T, Upper] => new Spline(nonEmpty)
    }
  }

  /** Change spline by modifying it's building points
    *
    * @param xy modifying (x, f(x)) function
    * @param builder implicit builder
    * @tparam B new piece function type
    * @return modified spline
    */
  def map[B <: PieceFunction](
        xy: (Double, Double) => Double)(
    implicit builder: PieceFunFactory[B]): Option[Spline[B]] = {
    Spline[B](
    points.map{t =>
      val (x, y) = t
      val newY = xy(x, y)
      (x, newY)
    }.toList)
  }

  /** Set of arguments of two splines
    *
    * @param spl other spline
    * @return
    */
  protected def sumArguments(spl: Spline[PieceFunction]): List[Double] = {
    val list =
      spl.iterator.flatMap(x => Iterator(x._1._1, x._1._2)) ++
      this.iterator.flatMap(x => Iterator(x._1._1, x._1._2))
    val min = math.max(this.content.low, spl.content.low)
    val max = math.min(this.content.upp, spl.content.upp)
    list.filter(x => x >= min && x <= max).to[SortedSet].toList
  }

  def /[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]): Option[Spline[B]] = {
    Spline(sumArguments(spl).map(x => (x, apply(x) / spl(x))))(builder)
  }

  def +[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]
  ): Option[Spline[B]] = {
    Spline(sumArguments(spl).map(x => (x, apply(x) + spl(x))))(builder)
  }

  def -[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]): Option[Spline[B]] = {
    Spline(sumArguments(spl).map(x => (x, apply(x) - spl(x))))(builder)
  }

  def *[B >: S <: PieceFunction](spl: Spline[PieceFunction])(
    implicit builder: PieceFunFactory[B]): Option[Spline[B]] = {
    Spline(sumArguments(spl).map(x => (x, apply(x) * spl(x))))(builder)
  }

  def convert[R <: PieceFunction](f: SplineConvert[S, R]): Spline[R] = {
    content.map((l: Double, u: Double, pf: S) => (l, u, f.apply(l, u, pf))) match {
      case nonEmpty: NonEmptyITree[Double, R, Upper] => new Spline[R](nonEmpty)
      case empty: EmptyNode[Double, R] => ???
    }
  }

  def splitNodes(f: (Double, Double, S) => Int): Spline[S] = {
    val result =
      content.iterator.flatMap{t =>
        val ((low: Double, upp: Double), s: S) = t
        val size = f(low, upp, s)
        val length = upp - low

        if (size == 1){
          Iterator.single(t)
        }
        else {
          val it0 =
            low.to(upp, length / size)
              .sliding(2)
              .map(seq => (seq(0), seq(1)))

          val it2 = Iterator.continually(s)
          it0.zip(it2)
        }
      }

    val (it, itt)= result.duplicate

    val size = itt.size

    IntervalTree.buildRight(it, size) match {
      case nonEmpty: NonEmptyITree[Double, S, Upper] => new Spline(nonEmpty)
      case empty: EmptyNode[Double, S] =>
        throw new RuntimeException("Something goes wrong..." +
          " split operation with non empty spline must have non empty result")
    }
  }



  def asUniSpline[S1 >: S <: PieceFunction, S2 >: S1 <: PieceFunction](
        implicit builder: PieceFunFactory[S1]): Spline[S2] =
    Spline.makeUniSpline[S1, S2](this)

  override def toString: String = {
    s"Spline(${content.toString})"
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case spl: Spline[PieceFunction] => content.equals(spl.content)
      case _ => false
    }
  }

  def intervalLength: Double = {
    import com.twitter.algebird.Group._
    content.intervalLength
  }

  def lowerBound: Double = content.lowerBound.lower

  def upperBound: Double = content.upperBound.upper

}
object Spline{

  private def makeUniSpline[S <: PieceFunction, S1 >: S <: PieceFunction](
        spline: Spline[S])(implicit factory: PieceFunFactory[S]): Spline[S1] = {
    import com.twitter.algebird._

    val source = factory.unapply(spline)

    IntervalTree.buildLeft[Double, S1](source, spline.content.size + 2) match {
      case nonEmpty: NonEmptyITree[Double, S1, Upper] => new Spline[S1](nonEmpty)
      case empty: EmptyNode[Double, S1] => {
        throw new RuntimeException("Something goes wrong..." +
          " Sources not empty apriori, because spline content non empty")
      }
    }
  }

  /** Bound points of spline
    *
    * @param spline spline, from which points is extracted
    * @tparam T type of spline
    * @return (lower x, upper x, lower y, upper y)
    */
  def boundsOf[T <: PieceFunction](spline: Spline[T])
  : (Double, Double, Double, Double) = {
    val lowerX = spline.lowerBound
    val upperX = spline.upperBound
    val lowerY = spline(lowerX)
    val upperY = spline(upperX)
    (lowerX, upperX, lowerY, upperY)
  }

  def const(xLow: Double, xUpp: Double, y: Double): Option[Spline[Const]] = {
      IntervalTree.buildOne(xLow, xUpp, new Const(y)) match {
        case empty: EmptyNode[Double, Const] => None
        case nonEmpty: NonEmptyITree[Double, Const, _] => {
          Some(new Spline(nonEmpty.asInstanceOf[NonEmptyITree[Double, Const, Upper]]))
        }
      }
  }

  def const(value: Double): Spline[Const] = {
    IntervalTree.buildOne(
      Double.MinValue,
      Double.MaxValue,
      new Const(value)) match {
      case _: EmptyNode[Double, Const] =>
        throw new RuntimeException("Result should not be empty")
      case nonEmpty: NonEmptyITree[Double, Const, Upper] => {
        new Spline(nonEmpty.asInstanceOf[NonEmptyITree[Double, Const, Upper]])
      }
    }
  }

  def line(low: (Double, Double), upp: (Double, Double)): Option[Spline[Line]] = {
    line(low._1, low._2, upp._1, upp._2)
  }

  def line(xLow: Double, yLow: Double, xUpp: Double, yUpp: Double): Option[Spline[Line]] = {
    val l = Line(xLow, xUpp, yLow, yUpp)
    IntervalTree.buildOne(xLow, xUpp, Line(xLow, xUpp, yLow, yUpp)) match {
      case empty: EmptyNode[Double, Line] => None
      case nonEmpty: NonEmptyITree[Double, Line, Upper] => {
        Some(new Spline(nonEmpty.asInstanceOf[NonEmptyITree[Double, Line, Upper]]))
      }
    }
  }

  def lines(points: List[(Double, Double)]): Option[Spline[Line]] =
    Spline(points)(LinesFactory)

  def m1Hermite3(points: List[(Double, Double)]): Option[Spline[M1Hermite3]] = {
    Spline(points)(CubicHermiteMonotoneFactory)
  }

  def fHermite3(points: List[(Double, Double)]): Option[Spline[Hermite3]] = {
    Spline(points)(CubicHermiteFactory)
  }

  def smooth = ???

  def apply[S <: PieceFunction: PieceFunFactory](
            list: List[(Double, Double)]): Option[Spline[S]] = {
    val size = list.size
    val iter = list.sortBy(_._1).iterator
    apply(iter, size)(implicitly[PieceFunFactory[S]])
  }


  def apply[S <: PieceFunction: PieceFunFactory](iter: Iterator[(Double, Double)],
                                                 size: Int): Option[Spline[S]] = {
    val pieceFunctions = implicitly[PieceFunFactory[S]].apply(iter)

    if (pieceFunctions.isEmpty) {
      None
    }
    else {
      IntervalTree.buildRight(pieceFunctions, size - 1) match {
        case empty: EmptyNode[Double, S] => None
        case nonEmpty: NonEmptyITree[Double, S, Upper] => Some(new Spline(nonEmpty))
      }
    }
  }

  abstract class PieceFunFactory[P <: PieceFunction]{

    def apply(args: Iterator[(Double, Double)]): Iterator[((Double, Double), P)]

    def applyConst(x0: Double, x1: Double, y: Double): P

    def unapply[P1 >: P <: PieceFunction](spl: Spline[P]
                                         ): Iterator[((Double, Double), P1)] = {
      val (lowerX, upperX, lower, upper) = Spline.boundsOf(spl)

      val lowInterval = (Double.MinValue, lowerX)

      val uppInterval = (upperX, Double.MaxValue)

      val low = applyConst(lowInterval._1, lowInterval._2, lower)

      val upp = applyConst(uppInterval._1, uppInterval._2, upper)

      Iterator.single((lowInterval, low)) ++
      spl.content.iterator ++
      Iterator.single((uppInterval, upp))
    }

  }


  implicit case object CubicHermiteFactory extends PieceFunFactory[Hermite3] {

    override def apply(args: Iterator[(Double, Double)]
                      ): Iterator[((Double, Double), Hermite3)] = {
      Hermite3.applyIncremental(args)
    }


    override def applyConst(x0: Double, x1: Double, y: Double): Hermite3 = {
      val x0 = 0.0
      new Hermite3(Array(y, 0.0, 0.0, 0.0, 0.0), x0)
    }
  }

  implicit case object CubicHermiteMonotoneFactory extends PieceFunFactory[M1Hermite3] {

    override def apply(args: Iterator[(Double, Double)])
    : Iterator[((Double, Double), M1Hermite3)] = {
      M1Hermite3(args)
    }

    override def applyConst(x0: Double, x1: Double, y: Double): M1Hermite3 = {
      new M1Hermite3(Array(y, 0.0, 0.0, 0.0), x0)
    }
  }

  implicit case object LinesFactory extends PieceFunFactory[Line]{

    override def apply(args: Iterator[(Double, Double)])
    : Iterator[((Double, Double), Line)] = {
      Line(args)
    }

    def apply(xLow: Double, yLow: Double, xUpp: Double, yUpp: Double): Line = {
      Line.apply(xLow, xUpp, yLow, yUpp)
    }

    override def applyConst(x0: Double, x1: Double, y: Double): Line = new Line(0.0, y)

  }

  implicit object CubicLagrangeFactory extends PieceFunFactory[Lagrange3] {

    override def apply(args: Iterator[(Double, Double)]
                      ): Iterator[((Double, Double), Lagrange3)] = {
      Lagrange3(args.toList).map(s => ((s.low, s.upp), s))
    }.iterator


    override def applyConst(x0: Double, x1: Double, y: Double): Lagrange3 = {
      new Lagrange3(Array(y, 0.0, 0.0, 0.0), x0, x1)
    }
  }

  implicit case object MakeSquarePieceFunctions extends PieceFunFactory[Lagrange2]{

    override def apply(args: Iterator[(Double, Double)]
                      ): Iterator[((Double, Double), Lagrange2)] = {
      Lagrange2.apply(args)
    }

    override def applyConst(
    x0: Double, x1: Double, y: Double): Lagrange2 = {
      new Lagrange2(Array(y, 0.0, 0.0))
    }

  }

  implicit case object MakeConstPieceFunctions extends PieceFunFactory[Const]{

    override def apply(args: Iterator[(Double, Double)]
                      ): Iterator[((Double, Double), Const)] = {
      args.sliding(2).map{src: Seq[(Double, Double)] =>
        val Seq(low, upp) = src
        ((low._1, upp._1), new Const(low._2))
      }
    }

    override def applyConst(x0: Double, x1: Double, y: Double): Const = new Const(y)

  }

}
