package approximation
import org.specs2._
import org.specs2.mutable.Specification
import org.specs2.mutable._
import piecewise.{Line, PieceFunction, Spline}
import approximation._
import approximation.TwoDGrid._
/**
  *
  */
class TwoDGridUnit extends mutable.Specification{

  val xCoords = 1.0 to 10.0 by 1
  val yCoords = 1.0 to 10.0 by 1

  val leftX = xCoords.map(x => 0.0)
  val rightX = xCoords.map(x => 11.0)

  val leftY = yCoords.map(y => 0.0)
  val rightY = yCoords.map(y => 11.0)

  val conds: Array[Spline[PieceFunction]] =
    Array.fill(xCoords.length * yCoords.length)(
      Spline[Line]((-100.0, 1.0) :: (100.0, 1.0) :: Nil)
        .asInstanceOf[Spline[PieceFunction]])



}
