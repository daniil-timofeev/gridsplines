package piecewise
import org.specs2._

class HermitSpecs extends Specification{def is = s2"""

 Spline sources building procedures comparison with 4 points ${
  val vals = List((0.1, 0.2), (1.0, 2.0), (3.0, 4.0), (5.0, 6.0))

  val src0 = Hermite3.makeSources(vals.iterator).toList
  val src1 = MCSplineCheck.makeSources(vals).toList

  src0.map(_.sum).sum must be_==(src1.map(_.sum).sum)
}

Spline sources building procedures comparison with 3 points ${
  val vals = List((0.1, 0.2), (1.0, 2.0), (3.0, 4.0))

  val src0 = Hermite3.makeSources(vals.iterator).toList
  val src1 = MCSplineCheck.makeSources(vals).toList

  (src0.size must be_==(src1.size)) and
  (src0.map(_.sum).sum must be_==(src1.map(_.sum).sum))

}

 Spline apply many ${
  val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8), (40.0, 6.85)))
  val spl = Spline[Hermite3](points).get
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.8)) and
  (spl(35.0) must beBetween(6.8, 6.85))
}

 Spline apply 3 ${
  val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8)))
  val spl = Spline[Hermite3](points).get
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.9))
}

 Monothone spline apply many ${
   val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8), (40.0, 6.85)))
    val spl = Spline[M1Hermite3](points).get
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.8)) and
  (spl(35.0) must beBetween(6.8, 6.85))
 }

  Monothone spline apply 3 ${
   val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8)))
   val spl = Spline[M1Hermite3](points).get
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.8)) 
 }

  """

}
