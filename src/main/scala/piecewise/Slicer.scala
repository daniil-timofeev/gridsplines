package piecewise

/**
  * Created by Даниил on 20.04.2017.
  */
trait Slicer{

  type SliceType <: PieceFunction

  def sliceUpper(upper: Double): SliceType

  def sliceLower(lower: Double): SliceType
}
