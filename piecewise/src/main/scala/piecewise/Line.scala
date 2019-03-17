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

  override def antider(x: Double): Double = slope / 2.0 * x * x + intercept * x

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

  import cats.Monoid

  implicit val LineMonoid = new Monoid[Line]{

    override def combine(x: Line, y: Line): Line = {
      val newSlope = x.slope * y.slope
      val newIntercept = x.intercept * y.slope + y.intercept
      new Line(newSlope, newIntercept)
    }

    override def empty: Line = Line(1.0, 0.0)
  }

  def derivative(yUp: Double,
                 yL: Double,
                 xUp: Double,
                 xLow: Double): Double = (yUp - yL) / (xUp - xLow)

  def apply(points: Iterator[(Double, Double)]): Iterator[((Double, Double), Line)] = {
      points.sliding(2).collect{
        case Seq((xLow, yLow),(xUp, yUp)) =>
          ((xLow, xUp), apply(xLow, xUp, yLow, yUp))
      }
  }

  def apply(xLow: Double, xUp: Double, yLow: Double, yUp: Double): Line = {
    val slope = derivative(yUp, yLow, xUp, xLow)
    val intercept = PieceFunction.interpolate(xLow, xUp, yLow, yUp, 0.0)
    new Line(slope, intercept)
  }

  def convert[S <: PieceFunction](low: Double, upp: Double, fn: S): Line = {
    fn match {
      case line: Line => line
      case noLine => Line(low, upp, noLine(low), noLine(upp))
    }
  }



}
