object TestExampleSpline extends App{

  import piecewise._
  val temperatures = 100.0 to 200.0 by 10.0 toList

  // in kg/m3
  val density =
    List(0.598, 0.826, 1.121, 1.496, 1.966,
      2.547, 3.258, 4.122, 5.156, 6.397, 7.862)

  val points = temperatures zip density

  val spline = Spline.m1Hermite3(points).get

  spline(110)

  spline(150)

  spline(135)

  spline.der(185)

  spline.integral(185)

  spline.area(150, 185)


  spline.applyOption(250)

  spline.asUniSpline.applyOption(250)

  val capacitySources = temperatures zip List(
    2.135, 2.177, 2.206, 2.257, 2.315,
    2.395, 2.497, 2.583, 2.709, 2.586,
    3.023
  )

  val capacitySpline =
    Spline[M1Hermite3](capacitySources).get

  val volCapacitySpline = spline.*(capacitySpline).get

  println(volCapacitySpline.toString)

  println(volCapacitySpline(110))

  println(volCapacitySpline(150))

  println(volCapacitySpline(135))
}
