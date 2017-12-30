package piecewise

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}

/**
  * @author Daniil
  */
object SplineCheck extends Properties("Spline"){

  val pointGen = for {
    from <- choose(-100.0, 100.0)
    end <- choose(from, 100.0)
    x <- choose(from, end)
    y <- choose(0.0, 100.0)
  } yield (x, y)

  val doublesListGen: Gen[List[Double]] = for{
    g <- nonEmptyListOf[Double](choose(-100.0, 100.0))
  } yield g

  val doublePointsGen: Gen[List[(Double, Double)]] = for{
    x <- doublesListGen.map(_.distinct.sorted)
    xy <- x.map(l => (l, arbDouble.arbitrary.sample.get))
  } yield xy

  property(" Get building points") =
  forAll(doublePointsGen suchThat(list => list.lengthCompare(3) > 0)){
    (vals: List[(Double, Double)]) => {
      val spline = Spline[Line](vals)
      if (vals.size == 1) {
        propBoolean(spline.isEmpty)
      }
      else {
        spline.map(s => s.points.size).getOrElse(0) ?= vals.size
      }
    }}

  property("bounds") = forAllNoShrink(doublePointsGen suchThat(list => list.lengthCompare(3) > 0)){
    (vals: List[(Double, Double)]) => {
      val spline = Spline.lines(vals).get
      val (lowerX, upperX, lower, upper) = Spline.boundsOf(spline)
      val lowerX0 = spline.lowerBound
      val upperX0 = spline.upperBound
      val lower0 = spline(lowerX0)
      val upper0 = spline(upperX0)
      all(
        "Lower x" |:
        lowerX ?= lowerX0,
        "Upper x" |:
        upperX ?= upperX0,
        "Lower y" |:
        lower ?= lower0,
        "Upper y" |:
        upper ?= upper0
      )
    }
  }




  //property(" Cовпадение в точках при целых числах / coincidence in points with int numbers") = {
  //  forAll(intPointListGen ){points : List[(Int, Int)] => {
  //    val spline = Spline[MCSpline](points.map(vals => (vals._1.toDouble, vals._2.toDouble)), classOf[MCSpline])
  //    val nodes = spline.nodes
  //    val settedNodes = spline.nodes.map(_._2)
  //    val computedNodes = nodes.map((node : (Double, Double)) => spline.value(node._1))
  //     all(
  //       (settedNodes, computedNodes).zipped.map(error(_,_)).filter(_ > 3.0) ?=  List.empty[Double]
  //       ) :| "computedList: " + computedNodes.toString + ". settedList: " + settedNodes.toString()
  //   }}
  //}


  //property(" Совпадение в точках построения при действительных числах / coincidence in building points with double numbers") = {
  //  forAll(doublePointListGen) {
  //    points : List[(Double, Double)] => {
  //      val spline = Spline[MCSpline](points, classOf[MCSpline])
  //val nodes = spline.nodes
  //      val settedNodes = spline.nodes.map(_._2)
  //      val computedNodes = nodes.map((node : (Double, Double)) => spline.value(node._1))
  //      all(
  //        (settedNodes, computedNodes).zipped.map(error(_,_)).filter(_ > 3.0) ?= List.empty[Double]
  //      ) :| "computedList: " + computedNodes.toString + ". settedList: " + settedNodes.toString()
  //    }
  //  }
  //}

  //val intervalsGen = for{
  //  list <- ListGen.doublePointListGen
  //  intervalVal <- choose(list.head._1, list.last._1)
  //  intervalList <- listOfN[Double](1 , intervalVal)
  //  finalList <- listOfN[List[Double]](10, list.head._1 :: intervalList ::: list.last._1 :: Nil)
  //} yield {
  //  (finalList, list)
  //}


  //property(" Площадь графика считается однозначно / Square of spline calculated definitely") = {
  //  forAllNoShrink(intervalsGen) { raw : (List[List[Double]], List[(Double, Double)]) => {
  //    val intervals = raw._1.map(_.sorted)
  //   val points = raw _2
  //    val spline = Spline[MCSpline](points, classOf[MCSpline])
  //   val areas = intervals.map{interval => {
  //     (interval, interval.tail).zipped.map((from, to) => {
  //        spline.area(from, to)
  //      }).sum
  //    }
  //    }
  //  val errors = for(area1 <- areas; area2 <- areas if ListGen.error(area1, area2) > 3.0) yield {
  //      (area1, area2, ListGen.error(area1, area2))
  //    }
  //    errors ?= List.empty[(Double, Double, Double)]
  //  }
  //  }
 // }
}
