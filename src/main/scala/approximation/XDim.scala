package approximation

import piecewise.{PieceFunction, Spline}

class XDim(shape: Spline[PieceFunction], yDistr: Array[Double]) {

val rangeX: Array[Double] = ???
val lowerBoundX: Array[Double] = ???
val upperBoundX: Array[Double] = ???

val xBeginAt: Array[Int] = lowerBoundX.map{bound =>
  rangeX.indexWhere(x => bound < x)
}

val xEndAt: Array[Int] = upperBoundX.map{bound =>
  val positiveIdx = rangeX.indexWhere(x => bound < x)
  if (positiveIdx == -1) rangeX.size - 1
  else positiveIdx - 1
}

  def rowSize(idx: Int): Int = {
    xEndAt(idx) - xBeginAt(idx) + 1
  }

  class RowIterator{
    private val lastRow = lowerBoundX.size - 1
    private var gone = 0
    private var curIdx = xBeginAt(0)
    private var curRow = 0
    private var lastRowIdx = xEndAt(0)
    def reset: Unit = {
      curIdx = xBeginAt(0)
      curRow = 0
      gone = 0
    }
    def hasNext: Boolean = curIdx < lastRowIdx

    def posAtRow: Int = curIdx
    def rowIdx = curRow
    def next: Int = {
      curIdx += 1
      gone += 1
      gone
    }

    def hasNextRow: Boolean = curRow < lastRow
    def nextRow: Int = {
      curRow += 1
      curIdx = xBeginAt(curRow)
      gone += 1
      gone
    }
  }


}
