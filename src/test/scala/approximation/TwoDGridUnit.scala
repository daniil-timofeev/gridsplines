package approximation
import java.io.BufferedWriter

import org.specs2._
import org.specs2.mutable.Specification
import org.specs2.mutable._
import piecewise.{Line, PieceFunction, Spline}
import approximation._
import approximation.TwoDGrid._
import org.specs2.matcher._
/**
  *
  */
class TwoDGridUnit extends Specification{override def is = s2"""
      correctly update X  ${updateX}
      correctly update Y  ${updateY}
      correct horisontal iteration step ${horIter}
      correct vertical iteration step ${vertIter}
      correct full iteration step ${makeFullStep}
      no heat flow bound update at upper side ${noHeatFlowUpp}
      no heat flow bound update at lower side ${noHeatFlowLow}
      no heat flow bound update at right side ${noHeatFlowRight}
      no heat flow bound update at left  side ${noHeatFlowLeft}
      "write grid ${writeGridTest}
  """

  def updateX = {

    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)
    val bounds = Bounds(
      upp = new OneElementBound,
      low = new OneElementBound,
      left = new OneElementBound,
      right = new OneElementBound
    )

    val coef = (x: Double, y: Double) => Spline.const(-10.0, 10.0, 1.0)

    val grid = new TwoDGrid[Radial, Ortho](xD, yD, bounds, coef)

    grid.updateX(x => 1.0)

    grid.avValue must_== 1.0
  }

  def updateY = {

    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)
    val bounds = Bounds(
      upp = new OneElementBound,
      low = new OneElementBound,
      left = new OneElementBound,
      right = new OneElementBound
    )

    val coef = (x: Double, y: Double) => Spline.const(-10.0, 10.0, 1.0)

    val grid = new TwoDGrid[Radial, Ortho](xD, yD, bounds, coef)

    grid.updateY(y => 1.0)

    grid.avValue must_== 1.0
  }

  def makeGrid4Iter(): TwoDGrid[Radial, Ortho] = {
    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)
    val bounds = Bounds(
      upp = new OneElementBound,
      low = new OneElementBound,
      left = new OneElementBound,
      right = new OneElementBound
    )

    val coef = (x0: Double, x1: Double) => Spline.const(-10.0, 10.0, 1.0)
    new TwoDGrid[Radial, Ortho](xD, yD, bounds, coef)
  }

  def horIter = {
    val grid = makeGrid4Iter()
    grid *= 1.0
    grid.bounds *= 1.0
    grid.bounds.left *= 4.0
    grid.xIter(900)
    grid.grid.update()
    grid.noHeatFlow(Left)
    grid.noHeatFlow(Right)
    grid.bounds.left.get(0) must be_>(grid.bounds.right.get(0))
  }

  def vertIter = {
    val grid = makeGrid4Iter()
    grid *= 1.0
    grid.bounds *= 1.0
    grid.bounds.upp *= 4.0
    grid.yIter(900)
    grid.grid.update()
    grid.noHeatFlow(Upper)
    grid.noHeatFlow(Lower)
    grid.bounds.upp.get(0) must be_>(grid.bounds.low.get(0))
  }

  def makeFullStep = {
    val grid = makeGrid4Iter()
    grid.updateY(y => 1.0)
    grid.bounds *= 1.0
    grid.bounds.left *= 4.0
    grid.iteration(900.0)
    grid.avValue must be_>(1.0)
  }

  def makeGrid(): TwoDGrid[Radial, Ortho] = {
    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)
    val bounds = Bounds(
      upp = new OneElementBound,
      low = new OneElementBound,
      left = new OneElementBound,
      right = new OneElementBound
    )

    val coef = (x0: Double, x1: Double) => Spline.const(-10.0, 10.0, 1.0)
    new TwoDGrid[Radial, Ortho](xD, yD, bounds, coef)
  }

  def noHeatFlowLeft = {
    val grid = makeGrid()
    grid *= 1.0
    import TwoDGrid._
    grid.noHeatFlow(Left)
    grid.bounds.left.get(0) must_== 1
  }

  def noHeatFlowRight = {
    val grid = makeGrid()
    grid *= 1.0
    import TwoDGrid._
    grid.noHeatFlow(Right)
    grid.bounds.right.get(0) must_== 1
  }

  def noHeatFlowUpp = {
    val grid = makeGrid()
    grid *= 1.0
    import TwoDGrid._
    grid.noHeatFlow(Upper)
    grid.bounds.upp.get(0) must_== 1
  }

  def noHeatFlowLow = {
    val grid = makeGrid()
    grid *= 1.0
    import TwoDGrid._
    grid.noHeatFlow(Lower)
    grid.bounds.low.get(0) must_== 1
  }

  def writeGridTest = {
    import java.nio.file._

    val dest = Files.createTempFile(Paths.get("."), "WriteGrid", "temp")
    val grid = makeGrid()

    val writer = Files.newBufferedWriter(dest)
    try{
      grid.write(writer)
    } finally writer.close()


    val colsNum = grid.x.colsNum

    val read = Files.newBufferedReader(dest)
    val line =
    try {
      read.readLine()
    }
    finally {
      read.close()
      Files.deleteIfExists(dest)
    }
    line.split(" ").size must_== colsNum

  }

}
