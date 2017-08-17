import piecewise._

val points = (1.0, 2.03) :: (2.3, 4.5) :: (3.0, 6.0) :: (4.0, 7.0) :: Nil

val spline = Spline[M1Hermite3](points)
val lines =
spline.convert(Line.apply)