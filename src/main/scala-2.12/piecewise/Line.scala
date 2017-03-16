package piecewise
import com.twitter.algebird._
/** Just line at interval
  *
  */
case class Line(override val interval: Intersection[InclusiveLower, ExclusiveUpper, Double],
                yL : Double, yUp : Double) extends PieceFunction(interval){

  def this(val1: (Double, Double), val2: (Double, Double)){
    this(PieceFunction.makeInterval(val1._1, val2._1), val1._2, val2._2)
  }

  def this(x1: Double, y1: Double, x2: Double, y2: Double){
    this(PieceFunction.makeInterval(x1, x2), y1, y2)
  }

  private val intSize = interval.upper.upper - interval.lower.lower

  /** Swap x and y places
    *
    * @return x = f(y) spline
    */
  def swap: Line = {
    if(yL < yUp) Line(PieceFunction.makeInterval(yL, yUp), interval.lower.lower, interval.upper.upper)
    else Line(PieceFunction.makeInterval(yUp, yL), interval.upper.upper, interval.lower.lower)
  }

  override protected def extremum: List[Double] = List(interval.lower.lower, interval.upper.upper)


  override def derivative(x: Double): Double = (yUp - yL) / intSize


  //override def +(otherFunc: (A) forSome {type A <: PieceFunction}): (B) forSome {type B <: PieceFunction} = {
  //  otherFunc match {
  // case mcspline : MCSpline => mcspline.copy(f = mcspline.f + v(from),
  //      f1 = mcspline.f1 + v(to),
  //      d = mcspline.d + derivative(from),
  //      d1 = mcspline.d1 + derivative(to))
  //    case qpf : Lagrange2 => {
  //      val center = (from + to)/2.0
  //      val points : List[(Double, Double)] = List(from , center, to).map(x => (x, qpf.v(x) + v(x)))
  //      Lagrange2(points)
  //    }
  //    case lspl : LineSpline => this.copy(f = f + lspl.v(from), f1 = f1 + lspl.v(to))
  //  }
  //}


  //override def +(y: Double): (B) forSome {type B <: PieceFunction} = this.copy(f = f + y, f1 = f1 + y)


  //override def +(func: (Double) => Double): (B) forSome {type B <: PieceFunction} =
  //  this.copy(f = f + func(from), f1 = f1 + func(to))


  override def apply(x: Double): Double = k * x + nullVal

  override def integral(x: Double): Double = (yL + yUp) / 2.0 * intSize


  private[this]  val nullVal = PieceFunction.interpolate(interval.lower.lower, interval.upper.upper, yL, yUp, 0)
  private[this]  val k = derivative(interval.lower.lower)

  override def sliceTo(value: Double): Line = {
    val i = PieceFunction.sliceIntervalTo(value, interval)
    new Line(i, apply(i.lower.lower), apply(i.upper.upper))
  }

  override def sliceFrom(value: Double): Line = {
    val i = PieceFunction.sliceIntervalFrom(value, interval)
    new Line(i, apply(i.lower.lower), apply(i.upper.upper))
  }

  override def slice(from: Double, to: Double): Line = {
    val i = PieceFunction.sliceIntervalTo(to, PieceFunction.sliceIntervalFrom(from, interval))
    new Line(i, apply(i.lower.lower), apply(i.upper.upper))
  }
}
object Line{

  def apply(argVals: List[Double], funVals: List[Double]): Vector[Line] = {
    val argView = argVals.view
    val funView = funVals.view

    {argView.zip(argView.drop(1)) zip (funView.zip((funView.drop(1))))} map{t =>{
      val ((xLow, xUp),(yLow, yUp)) = t
      new Line(PieceFunction.makeInterval(xLow, xUp), yLow, yUp)
    }} toVector
  }

  def apply(points: List[(Double, Double)]): Vector[Line] = {
    val viewPoints = points.view
    (viewPoints zip (viewPoints drop 1)) map{p => {
      val ((x1, y1),(x2, y2)) = p
      new Line(PieceFunction.makeInterval(x1, x2), y1, y2)
    }} toVector
  }
}
