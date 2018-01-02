# Gridsplines

### Introdution
Project contains a finite difference solver for heat transfer and diffusion problems at one or two dimensional grids. The grid can represent orthogonal or cyllindric coordinate spaces.
There is a simple polynomial spline library also, which contains cubic Lagrange, cubic Hermite and monotone cubic Hermite polinomial splines. 

To add this library into your scala project,
add the following lines into ```build.sbt``` file:

```scala
val commonResolvers = Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at  "https://oss.sonatype.org/content/repositories/releases"
)

resolvers ++= commonResolvers

libraryDependencies += "com.github.daniil-timofeev" %% "gridsplines" % "0.2.0-SNAPSHOT"

```

## Splines

### Lagrange splines
...

### Monotonic splines
Monotonic Hermite splines is useful for two dimensional dataset interpolation, where exremum points are already known.
Spline will convert each of two adjacent points into a cubic polynomial equation, and extremum values will be only at spline construction points. Let's construct monotonic spline with folowing vapor physical properties data, which is temperature dependent.

| Temperature,  ºC | Density,  kg/m³ | Thermal capacity,  kJ/(kg·ºC) |
|------------------|-----------------|-------------------------------|
| 100              | 0.598           | 2.135                         |
| 110              | 0.826           | 2.177                         |
| 120              | 1.121           | 2.206                         |
| 130              | 1.496           | 2.257                         |
| 140              | 1.966           | 2.315                         |
| 150              | 2.547           | 2.395                         |
| 160              | 3.258           | 2.497                         |
| 170              | 4.122           | 2.583                         |
| 180              | 5.156           | 2.709                         |
| 190              | 6.397           | 2.586                         |
| 200              | 7.862           | 3.023                         |

```scala
import piecewise._

val temperatures = 100.0 to 200.0 by 10.0 toList

// in kg/m3
val density =
  List(0.598, 0.826, 1.121, 1.496, 1.966,
    2.547, 3.258, 4.122, 5.156, 6.397, 7.862)
    
    
val points = temperatures zip density
...

val spline = Spline.m1Hermite3(points)

// spline: piecewise.Spline[piecewise.M1Hermite3] = 
// Spline(
//  [100.0, 110.0): 0,0000*(x-100,0000000)^3 -0,0003*(x-100,0000000)^2  +0,0228*(x-100,0000000) +0,5980
//  [110.0120.0) :0,0000*(x-110,0000000)^3 +0,0003*(x-110,0000000)^2  +0,0262*(x-110,0000000) +0,8260
//  [120.0130.0) :0,0000*(x-120,0000000)^3 +0,0003*(x-120,0000000)^2  +0,0335*(x-120,0000000) +1,1210
//  [130.0140.0) :0,0000*(x-130,0000000)^3 +0,0004*(x-130,0000000)^2  +0,0422*(x-130,0000000) +1,4960
//  [140.0, 150.0): 0,0000*(x-140,0000000)^3 +0,0005*(x-140,0000000)^2  +0,0526*(x-140,0000000) +1,9660
//  [150.0160.0) :0,0000*(x-150,0000000)^3 +0,0005*(x-150,0000000)^2  +0,0646*(x-150,0000000) +2,5470
//  [160.0, 170.0): 0,0000*(x-160,0000000)^3 +0,0007*(x-160,0000000)^2  +0,0787*(x-160,0000000) +3,2580
//  [170.0180.0) :0,0000*(x-170,0000000)^3 +0,0007*(x-170,0000000)^2  +0,0949*(x-170,0000000) +4,1220
//  [180.0190.0) :0,0000*(x-180,0000000)^3 +0,0010*(x-180,0000000)^2  +0,1138*(x-180,0000000) +5,1560
//  [190.0, 200.0): -0,0001*(x-190,0000000)^3 +0,0022*(x-190,0000000)^2  +0,1353*(x-190,0000000) +6,3970
// )

spline(110)
// res0: Double = 0.826

spline(150)
// res1: Double = 2.547

spline(135)
// res2: Double = 1.718125
```
Spline values at construction point will stay without changes, while values of other point will be something between them.
It's also possible to calculate derivative and antiderivative at any point of spline interval. 
```scala

spline.der(185)
// res3: Double = 0.1238875000000001

spline.integral(185)
// res4: Double = 27.242786458333335
```
and calculate an area under the part of spline

```scala
spline.area(150, 185)
// res5: Double = 139.148203125
```
If you pass argument out of the spline interval, an error will be occured. 
If this case is possible in your program, use `spline.applyOption()` method instead. You can also convert this spline using `.asUniSpline` method, and then you get interval bound value, when passing an argument out of the interval bounds:

```scala
spline.applyOption(250)
// res6: Option[Double] = None

spline.asUniSpline.applyOption(250)
// res7: Option[Double] = Some(7.862000000000001)

```
WIP
