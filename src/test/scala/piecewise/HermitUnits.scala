package piecewise
import org.specs2._
import piecewise._
import org.specs2.matcher._

class HermitUnits extends Specification{def is = s2"""

 Spline apply many ${
  val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8), (40.0, 6.85)))
  val spl = Spline[Hermite3](points)
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.8)) and
  (spl(35.0) must beBetween(6.8, 6.85))
}



 Spline apply 3 ${
  val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8)))
  val spl = Spline[Hermite3](points)
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.9))
}

 Monothone spline apply many ${
   val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8), (40.0, 6.85)))
    val spl = Spline[M1Hermite3](points)
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.8)) and
  (spl(35.0) must beBetween(6.8, 6.85))
 }

  Monothone spline apply 3 ${
   val points = (List((-12.0, 5.75), (10.00, 6.7), (32.0, 6.8)))
   val spl = Spline[M1Hermite3](points)
  (spl(8.0) must beBetween(5.75, 6.7)) and
  (spl(25.0) must beBetween(6.7, 6.8)) 
 }

  """

}
