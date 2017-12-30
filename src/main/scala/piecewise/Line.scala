package piecewise

/** Just line at interval
  *
  */
case class Line(slope: Double, intercept: Double) extends Lagrange{

  override protected val coefs: Array[Double] = Array(
    intercept,
    slope
  )

  override def derivative(x: Double): Double = slope
  /** Swap x and y places
    *
    * @return x = f(y) spline
    */
  def swap: Line = {
    new Line(1.0 / slope, - intercept * slope)
  }

  override protected def extremum(low: Double, upp: Double): List[Double] = ???

  override def apply(x: Double): Double = slope * x + intercept

  override def integral(x: Double): Double = slope / 2.0 * x * x + intercept * x

  override def area(x0: Double, x1: Double): Double =
    (apply(x1) + apply(x0)) / 2.0 * (x1 - x0)

  override def toString: String = f"${slope}%.20f*x + ${intercept}%.20f"

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case line: Line => this.slope.equals(line.slope) &&
        this.intercept.equals(line.intercept)
      case _ => false
    }
  }

}
object Line{

  import com.twitter.algebird._
  class LineMonoid extends Monoid[Line]{
    lazy val zero = Line(1.0, 0.0)

    override def plus(x: Line, y: Line): Line = {
      val newSlope = x.slope * y.slope
      val newIntercept = x.intercept * y.slope + y.intercept
      new Line(newSlope, newIntercept)
    }
  }

  def derivative(yUp: Double,
                 yL: Double,
                 upp: Double,
                 low: Double): Double = (yUp - yL) / (upp - low)

  def apply(argVals: List[Double], funVals: List[Double]): List[Line] = {
    val argView = argVals.view
    val funView = funVals.view

    {argView.zip(argView.drop(1)) zip (funView.zip((funView.drop(1))))} map{t =>{
      val ((xLow, xUp),(yLow, yUp)) = t
      val der = derivative(yUp, yLow, xUp, xLow)
      val free = PieceFunction.interpolate(xLow, xUp, yLow, yUp, 0.0)
      new Line(der, free)
    }} toList
  }

  def apply(points: List[(Double, Double)]): Iterator[Line] = {
    if (points.size > 1) {
      points.sliding(2).map{pSeq =>
        val Seq((xLow, yLow),(xUp, yUp)) = pSeq
        apply(xLow, xUp, yLow, yUp)
      }
    }
    else Iterator.empty
  }
  def apply(xLow: Double, xUp: Double, yLow: Double, yUp: Double): Line = {
    val der = derivative(yUp, yLow, xUp, xLow)
    val free = PieceFunction.interpolate(xLow, xUp, yLow, yUp, 0.0)
    new Line(der, free)
  }

 class LineConverter[S <: PieceFunction] extends SplineConvert[S, Line]{

   override def apply(low: Double, upp: Double, fn: S): Line = {
     fn match {
       case line: Line => line
       case noLine => Line(low, upp, noLine(low), noLine(upp))
     }
   }
 }

  implicit def line[S <: PieceFunction]: LineConverter[S] = {
    new LineConverter[S]
  }


}
