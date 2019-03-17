package piecewise

import cats.implicits.{catsKernelStdOrderForDouble, catsStdShowForDouble}
import cats.instances.double.catsKernelStdGroupForDouble
import piecewise.Spline._
import piecewise.intervaltree.{NonEmptyInt, _}

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/** Spline with piece function type `S`
  */
 class Spline[+S <: PieceFunction](
   protected val content: NonEmptyInt[Double, S]){


  /** Value of function at `x`.
    *
    * @note provide unsafe access to value.
    * In a case, where `x` out of the spline interval,
    * an exception will be thrown.
    *
    * @param x function argument
    * @return function value
    */
  def apply(x: Double): Double = AbsITree.unsafeValueAt(x, content).apply(x)


  /** Value of function at `x`.
    *
    * @note provide safe access to value.
    * Returns `None` in a case, where `x` out of the spline interval.
    *
    * @param x function argument
    * @return function value
    */
  def applyOption(x: Double): Option[Double] =
    AbsITree.valueAt(x, content).map(_.apply(x))


  /** Derivative of function at `x`.
    *
    * @note Unsafe method. In a case, when `x` out of the spline interval,
    * the result will be an Exception.
    *
    * @param x function argument
    * @return function derivative
    */
  def der(x: Double): Double = AbsITree.unsafeValueAt(x, content).der(x)


  /** Derivative of function at `x`.
    *
    * @note in a case, when `x` out of the spline interval, the result will be None.
    *
    * @param x function argument
    * @return function derivative
    */
  def derOption(x: Double): Option[Double] =
    AbsITree.valueAt(x, content).map(_.der(x))



  /** Antiderivative of function at `x`.
    *
    * @note Unsafe method. In a case, when `x` out of the spline interval,
    *       the result will be an Exception.
    * @param x function argument
    * @return function antiderivative
    */
  def antider(x: Double): Double = {
    AbsITree.unsafeValueAt(x, content).antider(x)
  }
  /** Antiderivative of function at `x`.
    *
    * @note in a case, when `x` out of the spline interval, the result will be None.
    *
    * @param x function argument
    * @return function antiderivative
    */
  def antiderOption(x: Double): Option[Double] = {
    AbsITree.valueAt(x, content).map(_.antider(x))
  }

  /** Average value of spline at [`a`, `b`] to domain.
    * `a` may be greater than `b`, or `b` greater than `a`
    *
    * @param a lower bound
    * @param b upper bound
    * @return
    */
  def average(a: Double, b: Double): Double = {
    val lower = math.min(a, b)
    val upper = math.max(a, b)
    area(lower, upper) / (upper - lower)
  }

  /** Area under the spline domain at [`lower` to `upper`]
    *
    * @param lower lower bound
    * @param upper upper bound
    * @return area under the spline domain
    */
  def area(lower: Double, upper: Double): Double = {
    AbsITree.viewFold(lower, upper)(content)(
      (l: Double, u: Double, fun: S) => fun.area(l, u)
    )
  }

  def map[R <: PieceFunction](f: S => R): Spline[R] =
    AbsITree.functorV[Double].fmap(content)(f) match {
      case nonEmpty: NonEmptyInt[Double, R] => new Spline(nonEmpty)
    }

  /** Spline sources with ((low, upp], func) format
    */
  def sources: List[((Double, Double), S)] = iterator.toList

  /** Spline sources iterator with ((low, upp], func) format
    *
    */
  def iterator: Iterator[((Double, Double), S)] =
    AbsITree.toArray(content).iterator.map(x => (x._1.unapply, x._2))


  /** Spline building points iterator with (x, f(x)) format
    */
  def points: Iterator[(Double, Double)] = {
    val i = iterator.map(_._1)
    val (a, b) = i.next()
    val xs = Iterator(a, b) ++ i.map(_._2)
    xs.map(x => (x, apply(x)))
  }

  /** Slice spline with new upper bound
    *
    * @param bound new upper bound
    * @return sliced spline
    */
  def sliceUpper(bound: Double): Option[Spline[S]] = {
    AbsITree.sliceUpper(bound)(content).value match {
      case EmptyInt => None
      case nonEmpty: NonEmptyInt[Double, S] => Some(new Spline(nonEmpty))
    }
  }

  /** Slice spline with new lower bound
    *
    * @param bound new lower bound
    * @return sliced spline
    */
  def sliceLower(bound: Double): Option[Spline[S]] = {
    AbsITree.sliceLower(bound)(content).value match {
      case EmptyInt => None
      case nonEmpty: NonEmptyInt[Double, S] => Some(new Spline(nonEmpty))
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

    AbsITree.buildLeft(wholePieces, wholeSize).value match {
      case EmptyInt => ???
      case nonEmpty: NonEmptyInt[Double, T] => new Spline(nonEmpty)
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
    Spline[B](points.map{t =>
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
    map(f)
  }

  def splitNodes(f: (Double, Double, S) => Int): Spline[S] = {
    val result = iterator.flatMap{t =>
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

    AbsITree.buildRight(it, size).value match {
      case nonEmpty: NonEmptyInt[Double, S] => new Spline(nonEmpty)
      case EmptyInt =>
        throw new RuntimeException("Something goes wrong..." +
          " split operation with non empty spline must have non empty result")
    }
  }

  def withMaxNodeLength(l: Double): Spline[S] = {
    import algebra.instances.all._
    import cats.kernel.instances.double.catsKernelStdOrderForDouble

    val src = AbsITree.toArray(content)
      .iterator.flatMap{
      case (i: CsdOpn[Double], v: S) => {
        CsdOpn.pure.sectionsWithMaxLength(l)(i).map(i => (i.unapply, v))
      }
      case (i: Csd[Double], v: S) => {
        val coll = Csd.pure.sectionsWithMaxLength(l)(i)
        val last = coll.last
        val resList =
          coll.dropRight(1).map(i => (i.unapply, v)) ::: (last.unapply, v) :: Nil
        resList.iterator
      }
    }
    AbsITree.build(src.toList) match {
      case nonEmpty: NonEmptyInt[Double, S] => new Spline(nonEmpty)
      case EmptyInt => throw new RuntimeException("Unreachable place")
    }
  }

  def asUniSpline[S1 >: S <: PieceFunction](
        implicit builder: PieceFunFactory[S1]): Spline[PieceFunction] =
    Spline.makeUniSpline[S1](this)

  override def toString: String = {
    s"Spline(${content.toString})"
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case spl: Spline[PieceFunction] => content.equals(spl.content)
      case _ => false
    }
  }

  def intervalLength: Double = Interval.length(AbsITree.wholeInterval(content))

  def lowerBound: Double = AbsITree.leastK(content).get

  def upperBound: Double = AbsITree.greatestK(content).get

  def to[S1 <: PieceFunction](
    implicit pieceFunFactory: PieceFunFactory[S1]): Spline[S1] = {
    Spline(points, size + 1).get
  }

}
object Spline{

  private def makeUniSpline[S <: PieceFunction](
        spline: Spline[S])(implicit factory: PieceFunFactory[S]): Spline[PieceFunction] = {

    val content = spline.content
    val mostLeftX = AbsITree.leastK(content).get
    val mostLeftY = AbsITree.leastV(content).get.apply(mostLeftX)
    val mostRightX = AbsITree.greatestK(content).get
    val mostRightY = AbsITree.greatestV(content).get.apply(mostRightX)

    val prepend =
      AbsITree.prepend[Double, PieceFunction](Double.MinValue, Const(mostLeftY))(
        spline.content
      )

    val append =
      AbsITree.append[Double, PieceFunction](
        Double.MaxValue, Const(mostRightY))(prepend)

    append match {
      case xs: NonEmptyInt[Double, PieceFunction] =>
        new Spline(xs)
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
      AbsITree.singleton(xLow, xUpp, new Const(y)) match {
        case single: UpBoundLeaf[Double, Const] => Some(new Spline(single))
        case EmptyInt => None
      }
  }

  def const(value: Double): Spline[Const] = {
    AbsITree.singleton(Double.MinValue, Double.MaxValue, new Const(value)) match {
      case x: UpBoundLeaf[Double, Const] => new Spline(x)
      case EmptyInt =>
        throw new RuntimeException("Runtime error at `Spline.const`." +
          " `Double.MinValue` can never be greater when `Double.MaxValue`")
    }
  }

  def line(low: (Double, Double),
           upp: (Double, Double)): Option[Spline[Line]] =
    line(low._1, low._2, upp._1, upp._2)

  def line(xLow: Double, yLow: Double,
           xUpp: Double, yUpp: Double): Option[Spline[Line]] = {
    val l = Line(xLow, xUpp, yLow, yUpp)
    AbsITree.singleton(xLow, xUpp, Line(xLow, xUpp, yLow, yUpp)) match {
      case EmptyInt => None
      case x: UpBoundLeaf[Double, Line] => Some(new Spline(x))
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

  def apply[S <: PieceFunction: PieceFunFactory](
            list: List[(Double, Double)]): Option[Spline[S]] = {
    val size = list.size
    val iter = list.sortBy(_._1).iterator
    apply(iter, size)(implicitly[PieceFunFactory[S]])
  }


  def apply[S <: PieceFunction: PieceFunFactory](iter: Iterator[(Double, Double)],
                                                 size: Int): Option[Spline[S]] = {
    val pieceFunctions = implicitly[PieceFunFactory[S]].apply(iter)
    if (pieceFunctions.isEmpty) None
    else AbsITree.build(pieceFunctions, size - 1) match {
        case EmptyInt => None
        case nonEmpty: NonEmptyInt[Double, S]=> Some(new Spline(nonEmpty))
      }
  }

  abstract class PieceFunFactory[P <: PieceFunction]{
    def apply(args: Iterator[(Double, Double)]): Iterator[((Double, Double), P)]
    def applyConst(x0: Double, x1: Double, y: Double): P
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
