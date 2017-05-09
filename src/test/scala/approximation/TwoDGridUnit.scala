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
    Array.fill(xCoords.length * yCoords.length)(Spline[Line]((-100.0, 1.0) :: (100.0, 1.0) :: Nil).asInstanceOf[Spline[PieceFunction]])

  "Row iteration must " >> {
    "not provide temperature differences in any column" >> {
      val grid: TwoDGrid[Radial, Ortho] =
        TwoDGrid.apply[Radial, Ortho](leftX.toArray, xCoords.toArray, rightX.toArray,
                                      leftY.toArray, xCoords.toArray, rightY.toArray,
                                      conds, 1.0)
      for(i <- 1 to 100){
        grid.stepLeftXBound(3600.0, leftX.map(x => 2.0).toArray)
      }



      val column = grid.col(5).toSeq
      val controlValue = column(0)


      column must contain((v: Double) => v must beCloseTo(controlValue, 0.1))
    }
  }




}
