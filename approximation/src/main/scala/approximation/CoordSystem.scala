package approximation

import approximation.CoordSystem._
import cats._

import scala.math._
sealed trait CoordSystem{

  def toPolar: Polar

  def toDecart: Decart

  def to[T <: CoordSystem : CoordsConvert] = implicitly[CoordsConvert[T]].apply(this)

  def +[T <: CoordSystem : CoordsConvert : Monoid](coords: T): T = {
    implicitly[Monoid[T]].combine(this.to[T], coords)
  }

}
case class Polar(radius: Double, angle: Double) extends CoordSystem{

  private lazy val correctAngle: Radians = double2Radians(angle)
    override def toPolar = this
  override def toDecart = new Decart(radius * cos(angle), radius * sin(angle))

  def rangeByAngle(angleStep: Double): Vector[Polar] = {
    {correctAngle.v to {correctAngle.v + math.Pi * 2.0} by angleStep}
      .view.map(x => new Polar(radius, x)).toVector
  }

  def rangeByAngleNumbers(angles: Int): Vector[Polar] = {
    val angleStep: Double = math.Pi * 2.0 / angles
    rangeByAngle(angleStep)
  }
  }
case class Decart(abscissa: Double, ordinate: Double) extends CoordSystem{
  override def toPolar = {
    def radius = hypot(abscissa, ordinate)
    def angle = atan2(abscissa, ordinate)
    new Polar(radius, angle)
  }
  override def toDecart = this
}
object CoordSystem{

  trait Angle extends Any

  class Radians(val v: Double) extends AnyVal with Angle
  class Degrees(val v: Double) extends AnyVal with Angle

  implicit final def double2Radians(value: Double): Radians = {
    if(value >= 2 * Pi) double2Radians(value - 2 * Pi)
    else new Radians(value)
  }

  implicit final def double2Degrees(value: Double): Degrees = {
    if(value >= 360) double2Degrees(value - 360.0)
    else new Degrees(value)
  }

  abstract class CoordsConvert[To <: CoordSystem]{
      def apply(coord: CoordSystem): To
  }

  implicit object ToPolar extends CoordsConvert[Polar]{
    override def apply(coord: CoordSystem): Polar = coord.toPolar
  }

  implicit object ToDecart extends CoordsConvert[Decart]{
    override def apply(coord: CoordSystem): Decart = coord.toDecart
  }

  implicit object PolarMonoid extends Monoid[Polar]{
    override val empty: Polar = new Polar(0.0, 0.0)
    override def combine(x: Polar, y: Polar): Polar = {
      DecartMonoid.combine(x.toDecart, y.toDecart).toPolar
    }
  }

  implicit object DecartMonoid extends Monoid[Decart]{
    override val empty: Decart = new Decart(0.0, 0.0)
    override def combine(x: Decart, y: Decart): Decart = {
      new Decart(x.abscissa + y.abscissa, x.ordinate + y.ordinate)
    }
  }
}

