package piecewise

import org.specs2._

class SplineSpecs extends Specification {def is = s2"""
  Get 2 building points ${buildingPoints2}
  Get 3 building points ${buildingPoints3}
  Split 2 nodes by 2 ${split2NodesBy2}
  Split 3 nodes by 2 ${split3NodesBy2}
  Split nodes ${splitNodes}
  Slice upper to two elements ${sliceUpperAt0}
  Slice upper to three elements ${sliceUpperAt1}
  Correctly define the spline bounds ${bounds}
  Slice lower to three elements ${sliceLowerAt0}
  Slice lower to one element ${sliceLowerAt1}
  """

  def buildingPoints2 = {

    val points = List((0.0, 1.0), (1.0, 3.0))
    val spline = Spline.lines(points).get

    (spline.points.toList.size must_== 2) and
    (spline.points.toList.last._2 must_== 3.0) and
    (spline.points.toList.last._1 must_== 1.0)
  }

  def buildingPoints3 = {

    val points = List((0.0, 1.0), (1.0, 2.0), (2.0, 3.0))
    val spline = Spline.m1Hermite3(points).get

    (spline.points.toList.size must_== 3) and
    (spline.points.toList.last._2 must_== 3.0) and
    (spline.points.toList.last._1 must_== 2.0)
  }

  def split2NodesBy2 = {
    val points = List((0.0, 10.0), (1.0, 10.0), (2.0, 10.0))
    val spline = Spline[Line](points).get

    val splitted = spline.splitNodes((_, _, _) => 2)
    splitted.size must_== spline.size * 2
  }

  def split3NodesBy2 = {
    val points = List((0.0, 10.0), (1.0, 10.0), (2.0, 10.0), (3.0, 10.0))

    val spline = Spline[Hermite3](points).get

    val splitted = spline.splitNodes((_, _, _) => 2)
    splitted.size must_== spline.size * 2
  }

  def splitNodes = {
    val points =
      List(
        (-1.21,-4.67951876353844E-73),
        (-0.283,-3.941225878167406E28),
        (-0.0474,8.761448420716819E-248),
        (5.837,-6.72148765184524E-285)
      )

    val spline = Spline[Line](points).get

    val splitted = spline.splitNodes((_, _, _) => 2)
    splitted.size must_== spline.size * 2
  }

  def sliceUpperAt0 = {
    val points = List((-1.2, 0.0), (-0.5, 0.0), (0.3, 0.0), (1.0, 0.0), (2.0, 0.0))

    val spline = Spline[Line](points).flatMap(_.sliceUpper(0.0))
    (spline must beSome) and
    (spline.get.size must_== 2)
  }

  def sliceUpperAt1 = {
    val points = List((-1.2, 0.0), (-0.5, 0.0), (0.3, 0.0), (1.0, 0.0), (2.0, 0.0))

    val spline = Spline.lines(points).flatMap(_.sliceUpper(0.9))
    (spline must beSome) and
    (spline.get.size must_== 3)
  }

  def sliceLowerAt0 = {
    val points = List((-1.2, 0.0), (-0.5, 0.0), (0.3, 0.0), (1.0, 0.0), (2.0, 0.0))

    val spline = Spline.lines(points).flatMap(_.sliceLower(0.0))
    (spline must beSome) and
    (spline.get.size must_== 3)
  }

  def bounds = {
    val points = List((-1.2, 1.0), (-0.5, 2.0), (0.3, 3.0), (1.0, 2.0), (2.0, 1.0))
    val spline = Spline[Line](points).get

    val lowerBoundX = spline.lowerBound
    val upperBoundX = spline.upperBound
    val lowerBoundY = spline(lowerBoundX)
    val upperBoundY = spline(upperBoundX)

    (lowerBoundX must_== -1.2) and
    (upperBoundX must_== 2.0) and
    (lowerBoundY must beCloseTo(1.0, 0.000000001)) and
    (upperBoundY must beCloseTo(1.0, 0.000000001))

  }

  def sliceLowerAt1 = {
    val points = List((-1.2, 0.0), (-0.5, 0.0), (0.3, 0.0), (1.0, 0.0), (2.0, 0.0))

    val spline = Spline[Line](points).flatMap(_.sliceLower(1.0))
    (spline must beSome) and
    (spline.get.size must_== 1)
  }




}
