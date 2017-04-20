package piecewise
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird._
import minimization.GoldenSlice
/** Just line at interval
  *
  */
case class Line(d: Double, free: Double) extends Lagrange with Slicer{

    type SliceType = Line


  def sliceUpper(upper: Double): SliceType = this

  def sliceLower(lower: Double): SliceType = this

  override protected val coefs: Array[Double] = Array(
    free,
    d
  )


  override def derivative(x: Double): Double = d
  /** Swap x and y places
    *
    * @return x = f(y) spline
    */
  def swap: Line = {
    new Line(1.0 / d, - free * d) // TODO test me
  }

  override protected def extremum: List[Double] = ???


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


  override def apply(x: Double): Double = d * x + free

  override def integral(x: Double): Double = d / 2.0 * x + free * x

}
object Line{

  def derivative(yUp: Double, yL: Double, upp: Double, low: Double): Double = (yUp - yL) / (upp - low)

  def apply(argVals: List[Double], funVals: List[Double]): Vector[Line] = {
    val argView = argVals.view
    val funView = funVals.view

    {argView.zip(argView.drop(1)) zip (funView.zip((funView.drop(1))))} map{t =>{
      val ((xLow, xUp),(yLow, yUp)) = t
      val der = derivative(yUp, yLow, xUp, xLow)
      val free = PieceFunction.interpolate(xLow, xUp, yLow, yUp, 0.0)
      new Line(der, free)
    }} toVector
  }

  def apply(points: List[(Double, Double)]): Vector[Line] = {
    val viewPoints = points.view
    (viewPoints zip (viewPoints drop 1)) map{p => {
      val ((xLow, yLow),(xUp, yUp)) = p
      val der = derivative(yUp, yLow, xUp, xLow)
      val free = PieceFunction.interpolate(xLow, xUp, yLow, yUp, 0.0)
      new Line(der, free)
    }} toVector
  }



}
