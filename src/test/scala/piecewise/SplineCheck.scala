package piecewise

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


  val doublePointsGen: Gen[List[(Double, Double)]] =
    doublesListGen.map(_.distinct.sorted)
      .flatMap((x: List[Double]) => {
        x.map(x => choose(-100.0, 100.0).map(y => List((x, y)))).reduce((a, b) => {
          a.flatMap((a: List[(Double, Double)]) =>
            b.map((b: List[(Double, Double)]) => a ++ b))
        })
      })

  property(" Get lines building points") =
  forAllNoShrink(doublePointsGen suchThat(list => list.lengthCompare(3) > 0)){
    (vals: List[(Double, Double)]) => {
      val spline = Spline[Line](vals)
      if (vals.size == 1) {
        propBoolean(spline.isEmpty)
      }
      else {
        spline.map(s => s.points.size).getOrElse(0) ?= vals.size
      }
    }}

  property("bounds") =
    forAllNoShrink(
      doublePointsGen.map(_.sortBy(_._1)) suchThat(list => list.lengthCompare(3) > 0)
    ){(vals: List[(Double, Double)]) => {
      val spline = Spline.lines(vals).get
      val lowerX = spline.lowerBound
      val upperX = spline.upperBound
      val lower = spline(lowerX)
      val upper  = spline(upperX)
      val lowX = vals(0)._1
      val low = vals(0)._2
      val uppX = vals.last._1
      val upp = vals.last._2
      all(
        "Lower x" |: {propBoolean(lowerX / lowX < 1.0001)},
        "Upper x" |: {propBoolean(upperX / uppX < 1.0001)},
        "Lower y" |: {propBoolean(lower / low < 1.0001)},
        "Upper y" |: {propBoolean(upper / upp < 1.0001)}
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
