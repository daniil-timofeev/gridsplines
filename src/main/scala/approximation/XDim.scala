package approximation

import approximation.TwoDGrid.Grid
import approximation.XDim.RowIterator
import piecewise.{PieceFunction, Spline}

case class XDim[T <: TypeDir] private(
                                  low: Double,
                                  range: Array[Double],
                                  upp: Double)(implicit val t: T)
extends Dim[T]{

  def this(lowX: Double, increment: (Double) => Double, uppX: Double)(
    implicit t: T){
    this(lowX,
        Iterator.iterate(increment(lowX))(increment).takeWhile(_ < uppX).toArray,
        uppX)
  }

  override def toString: String = {
    range.map{x => f"${x}%.3f"}.mkString(
        f"X/Y\t\t|\t${low}%.3f\t|\t",
        "\t",
        f"\t|\t${upp}%.3f\t|\t..." + System.lineSeparator()
      )
  }

  val colsNum = range.length
  def rowSize(idx: Int): Int = {
    colsNum
  }

  def idx(r: Int, c: Int): Int = {
    colsNum * r + c
  }


}
object XDim{

  class RowIterator(x: XDim[_], y: YDim[_]) extends IterOps{

    private val lastRow = y.rowsNum - 1
    private var arrayIdx = -1
    private var atRowIdx = 0
    private var curRow = -1
    private val lastColIdx = x.colsNum - 1

    override final def reset: Unit = {
      curRow = -1
      arrayIdx = -1
    }

    override final def hasNext: Boolean = atRowIdx < lastColIdx
    override final def hasTwoNext: Boolean = atRowIdx < lastColIdx - 1

    override final def hasPrev: Boolean = atRowIdx != 0

    override def prev: Int = {
      arrayIdx -= 1
      atRowIdx -= 1
      arrayIdx
    }

    final def posAtRow: Int = atRowIdx
    final def rowIdx = curRow
    final def rowCoord = y.coord(curRow)

    override final def next: Int = {
      arrayIdx += 1
      atRowIdx += 1
      arrayIdx
    }


    def hasNextRow: Boolean = curRow < lastRow
    def nextRow: Int = {
      curRow += 1
      atRowIdx = 0
      arrayIdx = curRow * y.rowsNum
      arrayIdx
    }

    def toRowBeginning(): Unit = {
      arrayIdx -= atRowIdx - 1
      atRowIdx = 0
    }

    override final def hasNextLayer: Boolean = hasNextRow

    override final def nextLayer: Int = nextRow

    override final def layer: Int = rowIdx

    override def posAtLayer: Int = atRowIdx
  }
}
