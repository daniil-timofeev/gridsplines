package piecewise

abstract class SplineConvert[-S <: PieceFunction, +R <: PieceFunction]
  extends Function3[Double, Double, S, R]{

}
