package piecewise
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird._
import minimization.GoldenSlice
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
    new Line(1.0 / slope, - intercept * slope) // TODO test me
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


  override def apply(x: Double): Double = slope * x + intercept

  override def integral(x: Double): Double = slope / 2.0 * x * x + intercept * x

  override def roughArea(x0: Double, x1: Double): Double =
    (apply(x1) + apply(x0)) / 2.0 * (x1 - x0)

  override def toString: String = f"${slope}%.15f*x + ${intercept}%.15f"

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case line: Line => this.slope.equals(line.slope) &&
        this.intercept.equals(line.intercept)
      case _ => false
    }
  }

}
object Line{

  def derivative(yUp: Double, yL: Double, upp: Double, low: Double): Double = (yUp - yL) / (upp - low)

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
    points.sliding(2).map{pSeq =>
      val Seq((xLow, yLow),(xUp, yUp)) = pSeq
      apply(xLow, xUp, yLow, yUp)
    }
  }
  import com.twitter.algebird.AffineFunction
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
