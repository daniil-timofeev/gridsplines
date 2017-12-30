package piecewise
import org.specs2._

class UniSplineSpecs extends Specification {def is = s2"""
  Defined at all field of doubles ${isDefined}
  Defined at lower bound ${isDefinedAtLowerBound}
  Defined at upper bound ${isDefinedAtUpperBound}
  """

  val points: List[(Double, Double)] = {(-100.0, 0.0) :: (-30.0, 15.0) ::
      (40.0, 25.0) :: (50.0, 50.0) :: (100.0, 100.0) :: Nil}


  val spline = UniSpline[M1Hermite3](points).get

  def isDefined = {
    (spline(-200.0) must_== 0.0) and
    (spline(40.0) must_== 25.0) and
    (spline(120.0) must_== 100.0)
  }

  def isDefinedAtLowerBound = {
    spline(-100.0) must_== 0.0
  }

  def isDefinedAtUpperBound = {
    spline(100.0) must_== 100.0
  }
}
