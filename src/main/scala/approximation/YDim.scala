package approximation

case class YDim[T <: TypeDir](low: Double, range: Array[Double], upp: Double)(
  implicit val t: T)
  extends Dim[T]{

  def this(lowY: Double, increment: (Double) => Double, uppY: Double)(
          implicit t: T
  ){
    this(lowY,
      Iterator.iterate(increment(lowY))(increment).takeWhile(_ < uppY).toArray,
      uppY)
  }

  def colSize(idx: Int): Int = {
    range.length
  }

  val rowsNum = range.length


}
object YDim{

  class ColumnIterator(x: XDim[_], y: YDim[_]) extends IterOps{

    private val lastCol = x.colsNum - 1
    private var arrayIdx = -1
    private var atColIdx = 0
    private var curCol = -1
    private val lastIdx = y.rowsNum * lastCol
    private val preLastIdx = y.rowsNum * (lastCol - 1)

    def reset: Unit = {
      curCol = -1
      arrayIdx = 0
    }

    override
    final def hasNext: Boolean = arrayIdx < lastIdx + curCol
    override
    final def hasTwoNext: Boolean = arrayIdx < preLastIdx + curCol
    override
    final def hasPrev: Boolean = atColIdx != 0

    override final def prev: Int = {
      arrayIdx -= x.colsNum
      atColIdx -= 1
      arrayIdx
    }
    final def posAtCol: Int = atColIdx
    final def colIdx = curCol
    final def colCoord = x.coord(curCol)
    override final def next: Int = {
      arrayIdx += x.colsNum
      atColIdx += 1
      arrayIdx
    }

    def hasNextCol: Boolean = curCol < lastCol
    def nextCol: Int = {
      curCol += 1
      atColIdx = 0
      arrayIdx = curCol
      arrayIdx
    }

    def toColBeginning(): Unit = {
      arrayIdx = curCol - 1
      atColIdx = 0
    }

    override final def hasNextLayer: Boolean = hasNextCol

    override final def nextLayer: Int = nextCol

    override final def layer: Int = colIdx

    override final def posAtLayer: Int = atColIdx
  }

}
