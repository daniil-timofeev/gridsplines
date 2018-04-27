package approximation

import org.specs2.mutable.Specification

/**
  *
  */
class TwoDGridUnit extends Specification{override def is = s2"""
      correctly update X  ${updateX}
      correctly update Y  ${updateY}
      correct horizontal iteration step ${horIter}
      correct vertical iteration step ${vertIter}
      correct full iteration step ${makeFullStep}
      no heat flow bound update at upper side ${noHeatFlowUpp}
      no heat flow bound update at lower side ${noHeatFlowLow}
      no heat flow bound update at right side ${noHeatFlowRight}
      no heat flow bound update at left  side ${noHeatFlowLeft}
      "write grid ${writeGridTest}
      "x coefficient array size ${getXCoefs}"
      "y coefficient array size ${getYCoefs}"
      "Orthogonal direction will heated faster than radial ${radialAndOrthoHeating}"
  """

  def updateX = {

    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)

    val coef = new ConstantCoef(Spline.const(1.0), Spline.const(1.0), Spline.const(1.0))

    val grid = TwoDGrid(xD, yD)(One, Temp)(One, Temp)(One, Temp)(One, Temp)(coef)

    grid.updateX(x => 1.0)

    grid.avValue must_== 1.0
  }

  def updateY = {

    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)

    val coef = new ConstantCoef(Spline.const(1.0), Spline.const(1.0), Spline.const(1.0))
    val grid = TwoDGrid(xD, yD)(One, Temp)(One, Temp)(One, Temp)(One, Temp)(coef)

    grid.updateY(y => 1.0)

    grid.avValue must_== 1.0
  }

  def makeGrid4Iter(): TwoDGrid[Radial, Ortho, Const] = {

    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 12.0)

    val coef = new ConstantCoef(Spline.const(1.0), Spline.const(1.0), Spline.const(1.0))

    TwoDGrid(xD, yD)(One, Temp)(One, Temp)(One, Temp)(One, Temp)(coef)
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

  def makeGrid(): TwoDGrid[Radial, Ortho, Const] = {
    val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
    val yD = new YDim[Ortho](1.0, y => y + 1.0, 12.0)

    val coef = new ConstantCoef(Spline.const(1.0), Spline.const(1.0), Spline.const(1.0))
    TwoDGrid(xD, yD)(One, Temp)(One, Temp)(One, Temp)(One, Temp)(coef)
  }

  def noHeatFlowLeft = {
    val grid = makeGrid()
    grid *= 1.0
    grid.noHeatFlow(Left)
    grid.bounds.left.get(0) must_== 1
  }

  def noHeatFlowRight = {
    val grid = makeGrid()
    grid *= 1.0
    grid.noHeatFlow(Right)
    grid.bounds.right.get(0) must_== 1
  }

  def noHeatFlowUpp = {
    val grid = makeGrid()
    grid *= 1.0
    grid.noHeatFlow(Upper)
    grid.bounds.upp.get(0) must_== 1
  }

  def noHeatFlowLow = {
    val grid = makeGrid()
    grid *= 1.0
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
    line.split(" ").size must_== {colsNum + 1}

  }

  def getXCoefs = {
    val grid = makeGrid()

    val xCoefs = grid.colCoefs(5)
    xCoefs.size must_== grid.y.rowsNum
  }

  def getYCoefs = {
    val grid = makeGrid()
    val yCoefs = grid.rowCoefs(5)
    yCoefs.size must_== grid.x.colsNum
  }


  def radialAndOrthoHeating = {

    def buildGrid = {

      val xD = new XDim[Radial](1.0, x => x + 1.0, 10.0)
      val yD = new YDim[Ortho](1.0, y => y + 1.0, 10.0)

      val coef = new ConstantCoef(Spline.const(1.5), Spline.const(1E6))

      val grid = TwoDGrid(xD, yD)(One, Flow)(One, Flow)(One, Flow)(One, Flow)(coef)

      grid.updateX(x => 6.0)
      grid.bounds *= 0.0
      grid
    }

    val timeStep = 15.0 * 60.0

    val grid = buildGrid

    for (i <- 1 to 100){
      grid.bounds.left *= 15.0
      grid.iteration(timeStep)
    }

    val grid0 = buildGrid

    for (i <- 1 to 100){
      grid0.bounds.upp *= 15.0
      grid0.iteration(timeStep)
    }

    import com.twitter.algebird._

    val verticalSum = grid0.col(5).map(AveragedValue(_)).reduce(_ + _).value
    val horizontalSum = grid.row(5).map(AveragedValue(_)).reduce(_ + _).value
    verticalSum must be_>(horizontalSum) //because vertical is ortho, and horizontal is radial
  }

}
