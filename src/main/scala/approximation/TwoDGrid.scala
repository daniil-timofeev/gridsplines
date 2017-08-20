package approximation

import approximation.TwoDGrid.{BaseBound, BoundSide, Bounds, Left, Lower, OneElementBound, Right, Upper}
import approximation.XDim.RowIterator
import approximation.YDim.ColumnIterator
import piecewise.{PieceFunction, Spline}

import scala.math.abs

class TwoDGrid[XType <: TypeDir, YType <: TypeDir](
        private val x: XDim[XType], private val y: YDim[YType],
        val bounds: Bounds,
        val coefs: (Double, Double) => Spline[PieceFunction]
){

  val grid = TwoDGrid.makeArray(x, y)

  def updateX(f: Double => Double) = {
    val iter = new ColumnIterator(x, y)
    while (iter.hasNextCol) {
      val fI = iter.nextCol
      val value = f(iter.colCoord)
      grid *= (fI, value)
      while (iter.hasNext){
        val i = iter.next
        grid *= (i, value)
      }
    }
  }

  def updateY(f: Double => Double) = {
    val iter = new RowIterator(x, y)
    while(iter.hasNextRow){
      val fI = iter.nextRow
      val value = f(iter.rowCoord)
      grid *= (fI, value)
      while (iter.hasNext){
        val i = iter.next
        grid *= (i, value)
      }
    }
  }

  def avValue: Double = grid.avValue

  def *=(v: Double) = grid *= v

  override def toString: String = {
    val buffer = new StringBuffer()

    val xCoords = x.toString

    buffer.append(xCoords)

    val lowBound =
      x.range.indices.map{i => f"${bounds.low.get(i)}%.3f"}
        .mkString(
          f"${y.low}%.3f\t|\t...\t\t|\t",
          "\t",
          f"\t|\t...\t\t|\t${y.upp}%.3f" + System.lineSeparator()
        )

    val uppBound =
      x.range.indices.map{i => f"${bounds.upp.get(i)}%.3f"}
      .mkString(
    f"${y.upp}%.3f\t|\t...\t\t|\t",
    "\t",
    f"\t|\t...\t\t|\t${y.upp}%.3f" + System.lineSeparator()
    )

    buffer.append(lowBound)
    val iter = new RowIterator(x, y)
    val g = grid.grid
    val rowArray = new Array[Double](x.rowSize(0))
    while (iter.hasNextRow){
      val fI = iter.nextRow
      rowArray.update(0, g(fI))
      while (iter.hasNext) {
        val i = iter.next
        rowArray.update(iter.posAtRow, g(i))
      }
      buffer.append(
        rowArray.map(d => f"${d}%.3f").mkString(
          f"${iter.rowCoord}%.3f\t|\t${bounds.left.get(iter.rowIdx)}%.3f\t|\t",
          "\t",
          f"\t|\t${bounds.right.get(iter.rowIdx)}%.4f\t|\t${iter.rowCoord}%.3f" +
            System.lineSeparator()
        )
      )
    }

    buffer.append(uppBound)
    buffer.append(xCoords)
    buffer.toString
  }

  def xIter(time: Double) = {
    val iter = new XDim.RowIterator(x, y)
    while (iter.hasNextRow) {
      val fI = iter.nextRow
      val t1 = bounds.left.get(iter.rowIdx)
      var t2 = grid(fI)
      var t3 = grid(fI + 1)
      var t = grid.get(fI)
      var c0 = x.first(time, t1, t2, t3, t,
        coefs(x.coord(iter.posAtRow), y.coord(iter.rowIdx)))

      while (iter.hasTwoNext) {
        val i = iter.next
        t2 = grid(i)
        t3 = grid(i + 1)
        t = grid.get(i)
        c0 = x.general(iter, time, t2, t3, t, c0,
          coefs(x.coord(iter.posAtRow), y.coord(iter.rowIdx)))
    }
      val i = iter.next
      t2 = grid(i)
      t3 = bounds.right.get(iter.rowIdx)
      t = grid.get(i)
      x.last(iter, time, t2, t3, t, c0,
        coefs(x.coord(iter.posAtRow), y.coord(iter.rowIdx)))
      x.update(grid, iter)
  }
  }

  def yIter(time: Double): Unit = {
    val iter = new YDim.ColumnIterator(x, y)
    while (iter.hasNextCol) {
      val fI = iter.nextCol
      val t1 = bounds.upp.get(iter.colIdx)
      var t2 = grid(fI)
      var t3 = grid(fI + x.colsNum)
      var t = grid.res(fI)
      var c0 = y.first(time, t1, t2, t3, t, coefs(x.coord(iter.colIdx), y.coord(iter.posAtCol)))

      while (iter.hasTwoNext) {
        val i = iter.next
        t2 = grid(i)
        t3 = grid(i + x.colsNum)
        t = grid.res(i)
        c0 = y.general(iter, time, t2, t3, t, c0, coefs(x.coord(iter.colIdx), y.coord(iter.posAtCol)))
      }

      val i = iter.next
      t2 = grid(i)
      t3 = bounds.low.get(iter.colIdx)
      t = grid.res(i)
      y.last(iter, time, t2, t3, t, c0, coefs(x.coord(iter.colIdx), y.coord(iter.posAtCol)))
      y.update(grid, iter)
    }
  }

  def iteration(time: Double) = {
    xIter(time)
    yIter(time)
    grid.update()
  }

  def left(copyTo: Array[Double]): Unit = {
    val g = grid.grid
    var i = 0
    var j = 0
    while (i < x.colsNum * y.rowsNum) {
      copyTo.update(j, g(i))
      i += x.colsNum
      j += 1
    }
  }

  def leftAverage: Double = {
    val g = grid.grid
    val size = y.rowsNum
    var i = 0
    var sum = 0.0
    while (i < x.colsNum * y.rowsNum) {
      sum += g(i)
      i += x.colsNum
    }
    sum / size
  }

  def right(copyTo: Array[Double]): Unit = {
    val g = grid.grid
    var i = x.colsNum - 1
    var j = 0
    while (i < x.colsNum * y.rowsNum) {
      copyTo.update(j, g(i))
      i += x.colsNum
      j += 1
    }
  }

  def rightAverage: Double = {
    val g = grid.grid
    val size = y.rowsNum
    var i = x.colsNum - 1
    var sum = 0.0
    while (i < x.colsNum * y.rowsNum){
      sum += g(i)
      i += x.colsNum
    }
    sum / size
  }

  def upper(copyTo: Array[Double]): Unit = {
    val g = grid.grid
    var i = 0
    while (i != x.colsNum) {
      copyTo.update(i, g(i))
      i += 1
    }
  }

  def upperAverage: Double = {
    val g = grid.grid
    val size = x.colsNum
    var i = 0
    var sum = 0.0
    while (i != x.colsNum) {
      sum += g(i)
      i += 1
    }
    sum / size
  }

  def lower(copyTo: Array[Double]): Unit = {
    val g = grid.grid
    var i = x.colsNum * (y.rowsNum - 1)
    var j = 0
    while (i != x.colsNum * y.rowsNum) {
      copyTo.update(j, g(i))
      i += 1
      j += 1
    }
  }

  def lowerAverage: Double = {
    val g = grid.grid
    var i = x.colsNum * (y.rowsNum - 1)
    val size = x.colsNum
    var sum = 0.0
    while (i != x.colsNum * y.rowsNum) {
      sum += g(i)
      i += 1
    }
    sum / size
  }

  def noHeatFlow(side: BoundSide): Unit = {
    side match {
      case Upper => {
        bounds.upp match {
          case base: BaseBound => upper(base.array)
          case one: OneElementBound => one.update(upperAverage)
        }
      }
      case Lower => {
        bounds.low match {
          case base: BaseBound => lower(base.array)
          case one: OneElementBound => one.update(lowerAverage)
        }
      }
      case Right => {
        bounds.right match {
          case base: BaseBound => right(base.array)
          case one: OneElementBound => one.update(rightAverage)
        }
      }
      case Left => {
        bounds.left match {
          case base: BaseBound => left(base.array)
          case one: OneElementBound => one.update(leftAverage)
        }
      }
    }
  }

  noHeatFlow(Upper)
  noHeatFlow(Lower)
  noHeatFlow(Right)
  noHeatFlow(Left)
}
object TwoDGrid{

  def makeArray[XType <: TypeDir,
                YType <: TypeDir](
        xDim: XDim[XType],
        yDim: YDim[YType]): Grid = {
    val grid, predict, result = new Array[Double](xDim.colsNum * yDim.rowsNum)
    new Grid(grid, predict, result)
  }

  class Grid private[TwoDGrid] (val grid: Array[Double],
                                val predict: Array[Double],
                                result: Array[Double]){
    def get(i: Int): Double = grid(i)
    def res(i: Int): Double = result(i)
    def put(i: Int, value: Double): Unit = result.update(i, value)
    def getP(i: Int): Double = predict(i)
    def apply(i: Int): Double = predict(i)

    def update(): Unit = {
      var i = 0
      while (i < grid.length){
        predict.update(i, aVal(grid(i), result(i)))
        i += 1
      }
      System.arraycopy(result, 0, grid, 0, grid.length)
    }

    def avValue = {
      val size = grid.size
      grid.sum / size
    }

    def *=(v: Double): Unit = {
      var i = 0
      while (i < grid.length){
        grid.update(i, v)
        predict.update(i, v)
        i += 1
      }
    }

    def *=(i: Int, value: Double): Unit = {
      grid.update(i, value)
    }

    /** Переопределяет предположение о темепературе на следующем шаге
      * @param oldVal температуры до итерирования
      * @param newVal температуры после итерирования
      * @return предполагаемая температура на следующем шаге
      */
    @inline
    final def aVal(oldVal: Double, newVal: Double): Double = {
      val delta = newVal - oldVal
      if (abs(delta) > 1.5) newVal
      else newVal + delta / 2.0
    }

    @inline
    final def midVal(oldVal: Double, newVal: Double): Double = {
      (oldVal + newVal) / 2.0
    }

  }

  abstract class Bound{
    def get(i: Int): Double
    def update(values: Array[Double]): Unit
    def update(value: Double): Unit
    def *=(values: Array[Double]): Unit = update(values)
    def *=(value: Double): Unit = update(value)
  }

  class OneElementBound extends Bound{
    private var value = 0.0
    override def get(i: Int): Double = value
    override def update(values: Array[Double]): Unit = {
      val size = values.size
      var sum = 0.0
      var i = 0
      while(i != size){
        sum += values(i)
        i += 1
      }
      value = sum / size
    }

    override def update(el: Double): Unit = {
      value = el
    }
  }

  case class BaseBound(range: Array[Double])
    extends Bound{
    def this(size: Int){
      this(new Array[Double](size))
    }
    def this(side: BoundSide, xDim: XDim[_], yDim: YDim[_]){
      this(side.size(xDim, yDim))
    }
    private val size = range.length
    private val values = range.clone()
    override def get(i: Int): Double = values(i)
    override def update(vals: Array[Double]): Unit = {
      System.arraycopy(vals, 0, values, 0, size)
    }

    override def update(value: Double): Unit = {
      var i = 0
      while (i != values.length){
        values.update(i, value)
        i += 1
      }
    }

    def array: Array[Double] = values
  }

  case class Bounds(upp: Bound, low: Bound, right: Bound, left: Bound){
    def *= (value: Double) = {
      upp.update(value)
      low.update(value)
      right.update(value)
      left.update(value)
    }
  }
  object Bounds{
    def withOneElement: Bounds = {
      val upp, low, right, left = new OneElementBound
      new Bounds(upp, low, right, left)
    }
    def withArrays(x: XDim[_], y: YDim[_]): Bounds = {
      val upp = new BaseBound(Upper, x, y)
      val low = new BaseBound(Lower, x, y)
      val right = new BaseBound(Right, x, y)
      val left = new BaseBound(Left, x, y)
      new Bounds(upp, low, right, left)
    }

    def horArraysVertOneElement(x: XDim[_], y: YDim[_]): Bounds = {
      val upp, low = new OneElementBound
      val right = new BaseBound(Right, x, y)
      val left = new BaseBound(Left, x, y)
      new Bounds(upp, low, right, left)
    }

    def horOneElementVertArrays(x: XDim[_], y: YDim[_]): Bounds = {
      val right, left = new OneElementBound
      val upp = new BaseBound(Upper, x, y)
      val low = new BaseBound(Lower, x, y)
      new Bounds(upp, low, right, left)
    }
  }

    abstract class BoundSide{
      def size(x: XDim[_], y: YDim[_]): Int
    }
    case object Upper extends BoundSide{
      def size(x: XDim[_], y: YDim[_]): Int = {
        x.colsNum
      }
    }
    case object Lower extends BoundSide{
      def size(x: XDim[_], y: YDim[_]): Int = {
        x.colsNum
      }
    }
    case object Right extends BoundSide{
      def size(x: XDim[_], y: YDim[_]): Int = {
        y.rowsNum
      }
    }
    case object Left extends BoundSide{
      override def size(x: XDim[_], y: YDim[_]): Int = {
        y.rowsNum
      }
    }



  case class Shape(rows: Int, cols: Int)


}
