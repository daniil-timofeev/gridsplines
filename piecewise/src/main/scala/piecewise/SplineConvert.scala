package piecewise

abstract class SplineConvert[-S <: PieceFunction, +R <: PieceFunction]
  extends Function1[S, R]{
}
