package approximation
import org.specs2._

class BoundaryConditions extends Specification{def is = s2"""
                                               $hello
  """


  def hello = {
    val heatFlow0 = TwoDGrid.radialHeatFlow(10.0, 9.0, 0.08, 0.09, 1.5)
    val heatFlow1 = TwoDGrid.discHeatFlow(10.0, 9.0, 0.08, 0.09, 1.5)
    heatFlow0 must beCloseTo(heatFlow1, 0.1)
  }
}
