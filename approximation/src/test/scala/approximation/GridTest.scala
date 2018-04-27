package approximation

import approximation.TwoDGrid._
import org.specs2._
import piecewise._

class GridTest extends Specification{def is = s2"""
     ${test}
  """

    val power = 4500

    val cond = 1.5
    val heatCap = 2.2E6 //J/(m3 K)
    val time = 900

    def test  = {

      val left = 0.0001 / 2.0
      val mid = 0.0005 / 2.0
      val last = 0.001 / 2.0

      val xDim = new XDim[Radial](left,
          (x: Double) => {
            //if (x == left) last
            if (x == left) mid
            if (x == mid) last
            else x * 1.1
          },
          25.0
        )

      val yDim = new YDim[Ortho](0, (y: Double) => y + 2.0, 92.0)

      val conduct = TwoDGrid.VarXCoef(xDim,
        (x0, x1) =>  Spline.const(cond),
        (x0, x1) => Spline.const(heatCap),
        (x0, x1) => Spline.const(cond / heatCap)
      )

      val grid =
        TwoDGrid(xDim, yDim)(Many, Temp)(Many, Temp)(Many, Temp)(Many, Flow)(conduct)
      val endTime = 175000
      import scala.collection._
      val points = mutable.Buffer.apply[Array[Double]]()

      grid *= 7.0

      val leftArray, leftArray0 = new Array[Double](grid.y.range.length)
      var current = 0
      while (current <= endTime) {
        val heat = power / (92.0 * 2.0 * math.Pi)
        var i = 0
        while (i != leftArray.length) {
          grid.bounds.left.update(heat)
          i += 1
        }
        grid.left(leftArray)
        points += Array(math.log(current), leftArray(0))
        import TwoDGrid.Bounds._
        grid.noHeatFlow(Right)
        grid.noHeatFlow(Upper)
        grid.noHeatFlow(Lower)
        grid.iteration(900)
        current += time
      }
    //  import java.nio.file._
    //  val dest =
    //    Paths.get(
    //      "C:", "Users", "Даниил", "Documents",
    //      "Аспирантура", "Диссертация", "images",
    //      "GHEValidation", "snapshots", "simple.dat"
    //    )
    //  val writer = Files.newBufferedWriter(dest,
    //    StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    //
    //  import java.text._
    //  val numb = NumberFormat.getInstance(Locale.ROOT)
    //  numb.setGroupingUsed(false)
    //  numb.setMaximumFractionDigits(2)
    //  writer.write("#time\ttemp")
    // writer.newLine()
    // try {
    //    points.drop(1).foreach{lineVars =>
    //      val line = s"${numb.format(lineVars(0))}.0\t${numb.format(lineVars(1))}"
    //      writer.write(line)
    //      writer.newLine()
    //   }
    // } finally writer.close()

      import org.apache.commons.math3.stat.regression._

      val reg = new SimpleRegression()

      reg.addData(points.drop(1).toArray)

      val slope = reg.getSlope

      power / (4.0 * math.Pi * 92 * slope) must beCloseTo(cond, 0.1)
    }
}
