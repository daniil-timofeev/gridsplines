package approximation
import org.specs2._
import approximation._
import piecewise.Spline
import TwoDGrid._

class GridTest extends Specification{def is = s2"""
     ${test}
  """

    val power = 3200

    val cond = 1.5
    val heatCap = 2.2E6 //J/(m3 K)
    val time = 900

    def test  = {

      val left = 0.0
      val mid = 0.09 / 2.0
      val last = 0.18 / 2.0

      val xDim = new XDim[Radial](left,
          (x: Double) => {
            if (x == left) mid
            if (x == mid) last
            else x + 0.05
          },
          25.0
        )

      val yDim = new YDim[Ortho](0, (y: Double) => y + 1.0, 30.0)

      val conduct = TwoDGrid.VarXCoef(xDim, (x0, x1) => {
        if(x0 > left) Spline.const(cond / heatCap)
        else Spline.const(cond / heatCap)
      })

      val bounds = TwoDGrid.Bounds.horArraysVertOneElement(xDim, yDim)
      val grid = new TwoDGrid(xDim, yDim, bounds, conduct)
      val endTime = 175000
      import scala.collection._
      val points = mutable.Buffer.apply[Array[Double]]()

      grid *= 7.0

      val leftArray, leftArray0 = new Array[Double](grid.x.range.length)
      var current = 0
      while (current <= endTime) {
        grid.left(1, leftArray)
        grid.left(0, leftArray0)
        val heat = power / 29.0
        var i = 0
        while (i != leftArray.length) {

          val t = TwoDGrid.discTWithFlow(
              heat, leftArray(i), last, left, cond
            )

          val t0 = TwoDGrid.getRadialMidPoint(
              left, mid, last, cond, cond,  t, leftArray(i)
            )

          leftArray0.update(i, t0)
          leftArray.update(i, t)
          i += 1
        }

        grid.bounds.left *= leftArray
        grid.updateColumn(0, leftArray0)
        points += Array(current, grid.bounds.left.get(15))
        grid.noHeatFlow(Right)
        grid.noHeatFlow(Upper)
        grid.noHeatFlow(Lower)
        grid.iteration(900)
        current += time
      }
      import java.nio.file._
      val dest =
        Paths.get(
          "C:", "Users", "Даниил", "Documents",
          "Аспирантура", "Диссертация", "images",
          "GHEValidation", "snapshots", "simple.dat"
        )
      val writer = Files.newBufferedWriter(dest,
        StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

      import java.text._
      import java.util.Locale
      val numb = NumberFormat.getInstance(Locale.ROOT)
      numb.setGroupingUsed(false)
      numb.setMaximumFractionDigits(2)
      writer.write("#time\ttemp")
      writer.newLine()
      try {
        points.drop(1).foreach{lineVars =>
          val line = s"${numb.format(lineVars(0))}.0\t${numb.format(lineVars(1))}"
          writer.write(line)
          writer.newLine()
        }
      } finally writer.close()


     ok
    }
}
