package piecewise

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}

/**
  * @author Даниил
  */
object SplineTest extends Properties("Сплайн / Spline"){

  val pointGen = for {
    from <- choose(-100.0, 100.0)
    end <- choose(from, 100.0)
    x <- choose(from, end)
    y <- choose(0.0, 100.0)
  } yield (x, y)

  val listGen: Gen[List[(Double, Double)]] = for{
    g <- nonEmptyListOf[(Double, Double)](pointGen)
  } yield g


  property(" correct slicing * 2") =
    forAll(listGen){(vals: List[(Double, Double)]) =>
      val spline = Spline.lines(vals)
      val src1 = spline.sources
      val splitted = spline.splitWhere((_, _, _) => 2)
      val src2 = splitted.sources
      src2.size ?= src1.size * 2
    }

  property(" get building points") =
  forAll(listGen){(vals: List[(Double, Double)]) => {
      import piecewise._
      val spline = Spline[Line](vals)
      val points = spline.points
      vals == points
    }}



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
