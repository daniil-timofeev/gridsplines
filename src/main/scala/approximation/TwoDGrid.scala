package approximation

import java.io.{BufferedWriter, IOException}

import approximation.TwoDGrid.{
BaseBound, BoundSide, Bounds, Coefficients, HeatFlowBound,
Left, Lower, OneElementBound, Right, TemperatureBound, Upper
}
import approximation.XDim.RowIterator
import approximation.YDim.ColumnIterator
import piecewise.{PieceFunction, Spline}

import scala.math.abs

class TwoDGrid[XType <: TypeDir, YType <: TypeDir](
        val x: XDim[XType], val y: YDim[YType],
        val bounds: Bounds,
        val coefs: Coefficients
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
  import java.nio.file._

  @throws(classOf[IOException])
  def write(writer: BufferedWriter): Unit = {
    import java.text._
    import java.util.Locale
    val g = grid.grid
    val form = NumberFormat.getInstance(Locale.ROOT)
    form.setMaximumFractionDigits(3)
    val iter = new RowIterator(x, y)
    while (iter.hasNextRow){
      val fI = iter.nextRow
      writer.write(form.format(iter.rowCoord))
      writer.write(" ")
      writer.write(form.format(g(fI)))
      while (iter.hasNext) {
        val i = iter.next
        writer.write(" ")
        writer.write(form.format(g(i)))
      }
      writer.newLine()
    }
  }

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
    var row = 0
    while (row != y.rowsNum) {
      var col = 0
      var i = row * x.colsNum
      var t2 = grid(i)
      var t3 = grid(i + 1)
      var t = grid.get(i)
      var c0 =
      bounds.left match {
        case temp: TemperatureBound => {
          val t1 = temp.get(row)
          x.first(time, t1, t2, t3, t,
            coefs(-1, row),
            coefs(col, row)
          )
        }
        case heatFlow: HeatFlowBound => {
          val heat = heatFlow.get(row)
          x.firstHeatFlow(time, heat, t2, t3, t,
            coefs.thermConductivity(col, row),
            coefs.capacity(col, row)
          )
        }
      }

      col += 1
      i += 1
      while (col != x.colsNum - 1) {
        t2 = grid(i)
        t3 = grid(i + 1)
        t = grid.get(i)
        c0 = x.general(col, time, t2, t3, t, c0,
          coefs(col, row))
        col += 1
        i += 1
    }

      t2 = grid(i)
      t = grid.get(i)

      bounds.right match {
        case temp: TemperatureBound => {
          t3 = bounds.right.get(row)
          x.last(col, time, t2, t3, t, c0,
            coefs(col, row))
        }

        case heatFlow: HeatFlowBound => {
          x.lastHeatFlow(col, time, heatFlow.get(row), t2, t3, t,
            coefs.tempConductivity(col, row),
            coefs.capacity(col, row)
          )
        }
      }

      x.update(grid, row, x.colsNum)
      row += 1
  }
  }

  def yIter(time: Double): Unit = {
    var col = 0
    val colsNum = x.colsNum
    while (col != colsNum) {
      var i = col
      var row = 0
      val t1 = bounds.upp.get(col)
      var t2 = grid(i)
      var t3 = grid(i + colsNum)
      var t = grid.res(i)
      var c0 = y.first(time, t1, t2, t3, t,
        coefs(col, -1),
        coefs(col, row))

      i += colsNum
      row += 1
      while (row != y.rowsNum - 1) {
        t2 = grid(i)
        t3 = grid(i + colsNum)
        t = grid.res(i)
        c0 = y.general(row, time, t2, t3, t, c0,
          coefs(col, row))
        i += colsNum
        row += 1
      }

      t2 = grid(i)
      t = grid.res(i)

      bounds.low match {
        case temp: TemperatureBound => {
          t3 = bounds.low.get(col)
          y.last(row, time, t2, t3, t, c0,
            coefs(col, row))
        }
        case heatFlow: HeatFlowBound => {
          y.lastHeatFlow(row, time, heatFlow.get(col), t1, t3, t,
            coefs.tempConductivity(col, row),
            coefs.capacity(col, row)
          )
        }
      }

      y.update(grid, col, colsNum)
      col += 1
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

  def col(colNum: Int, copyTo: Array[Double]): Unit = {
    val g = grid.grid
    var i = colNum
    var j = 0
    while (i < x.colsNum * y.rowsNum) {
      copyTo.update(j, g(i))
      i += x.colsNum
      j += 1
    }
  }

  def updateColumn(colNum: Int, copyFrom: Array[Double]): Unit = {
    val g = grid.grid
    var i = colNum
    var j = 0
    while (i < x.colsNum * y.rowsNum) {
      g.update(i, copyFrom(j))
      i += x.colsNum
      j += 1
    }
  }

  def row(rowNum: Int, copyTo: Array[Double]): Unit = {
    val g = grid.grid
    var i = rowNum * x.colsNum
    var j = 0
    val end = i + x.colsNum
    while (i != end) {
      copyTo.update(j, g(i))
      i += 1
      j += 1
    }
  }

  def updateRow(rowNum: Int, copyFrom: Array[Double]): Unit = {
    var g = grid.grid
    var i = rowNum * x.colsNum
    var j = 0
    var end = i + x.colsNum
    while (i != end) {
      g.update(i, copyFrom(j))
      i += 1
      j += 1
    }
  }

  def rowCoefs(rowNum: Int): Array[Spline[PieceFunction]] = {
    val result = new Array[Spline[PieceFunction]](x.colsNum)
    var xInd = 0
    while (xInd != x.colsNum) {
      result.update(xInd, coefs(xInd, rowNum))
      xInd += 1
    }
    result
  }

  def colCoefs(colNum: Int): Array[Spline[PieceFunction]] = {
    val result = new Array[Spline[PieceFunction]](y.rowsNum)
    var yInd = 0
    while (yInd != y.rowsNum) {
      result.update(yInd, coefs(colNum, yInd))
      yInd += 1
    }
    result
  }

  def left(colNum: Int, copyTo: Array[Double]): Unit = {
    col(colNum, copyTo)
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

  def right(colNumFromRigth: Int, copyTo: Array[Double]): Unit = {
    col(x.colsNum - 1 - colNumFromRigth, copyTo)
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

  def upper(rowNum: Int, copyTo: Array[Double]): Unit = {
    row(rowNum, copyTo)
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

  def lower(rowFromLast: Int, copyTo: Array[Double]): Unit = {
    row(y.rowsNum - 1 - rowFromLast, copyTo)
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
          case base: BaseBound => upper(0, base.array)
          case one: OneElementBound => one.update(upperAverage)
        }
      }
      case Lower => {
        bounds.low match {
          case base: BaseBound => lower(0, base.array)
          case one: OneElementBound => one.update(lowerAverage)
        }
      }
      case Right => {
        bounds.right match {
          case base: BaseBound => right(0, base.array)
          case one: OneElementBound => one.update(rightAverage)
        }
      }
      case Left => {
        bounds.left match {
          case base: BaseBound => left(0, base.array)
          case one: OneElementBound => one.update(leftAverage)
        }
      }
    }
  }

  def updatePredicted(): Unit = {
    System.arraycopy(grid.grid, 0, grid.predict, 0, grid.grid.length)
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
                                val result: Array[Double]){
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

  trait HeatFlowBound extends Bound
  trait TemperatureBound extends Bound

  abstract class OneElementBound extends Bound{
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

  class OneElementTemperature extends OneElementBound with TemperatureBound
  class OneElementHeatFlow extends OneElementBound with HeatFlowBound


  class BaseBound(range: Array[Double])
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

  case class HeatFlow(range: Array[Double]) extends BaseBound(range) with HeatFlowBound {

    def this(size: Int){
      this(new Array[Double](size))
    }
    def this(side: BoundSide, xDim: XDim[_], yDim: YDim[_]){
      this(side.size(xDim, yDim))
    }
  }

  case class Temperature(range: Array[Double]) extends BaseBound(range) with TemperatureBound{

    def this(size: Int){
      this(new Array[Double](size))
    }
    def this(side: BoundSide, xDim: XDim[_], yDim: YDim[_]){
      this(side.size(xDim, yDim))
    }
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
      val upp, low, right, left = new OneElementTemperature
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
      val upp, low = new OneElementTemperature
      val right = new BaseBound(Right, x, y)
      val left = new BaseBound(Left, x, y)
      new Bounds(upp, low, right, left)
    }

    def horOneElementVertArrays(x: XDim[_], y: YDim[_]): Bounds = {
      val right, left = new OneElementTemperature
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

  abstract class Coefficients {
    def thermConductivity(x: Int, y: Int): Spline[PieceFunction]
    def capacity(x: Int, y: Int): Spline[PieceFunction]
    def tempConductivity(x: Int, y: Int): Spline[PieceFunction]
    def apply(x: Int, y: Int): Spline[PieceFunction]
    def contains(x: Int, y: Int): Boolean = true
  }

  class ConstantCoef(
    private val thermCond: Spline[PieceFunction],
    private val cap: Spline[PieceFunction],
    private val tempCond: Spline[PieceFunction]) extends Coefficients{
    def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = thermCond
    def capacity(x: Int, y: Int): Spline[PieceFunction] = cap
    def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = tempCond
    def apply(x: Int, y: Int): Spline[PieceFunction] = tempCond
    val contains = true
  }

  class VarYCoef(private val thermCond: Array[Spline[PieceFunction]],
                 private val cap: Array[Spline[PieceFunction]],
                 private val tempCond: Array[Spline[PieceFunction]]
                ) extends Coefficients{

    override def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): Spline[PieceFunction] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): Spline[PieceFunction] = tempCond(y + 1)
  }

  object VarYCoef{
    def apply(yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[PieceFunction],
              buildCapCond: (Double, Double) => Spline[PieceFunction],
              buildTempCond: (Double, Double) => Spline[PieceFunction]
             ): VarYCoef = {
      new VarYCoef(
        yDim.values.sliding(2).collect{
          case Array(y0, y1) => buildThermCond(y0, y1)
        }.toArray,
        yDim.values.sliding(2).collect{
          case Array(y0, y1) => buildCapCond(y0, y1)
        }.toArray,
        yDim.values.sliding(2).collect{
          case Array(y0, y1) => buildTempCond(y0, y1)
        }.toArray
      )
    }
  }

  class VarXCoef(private val thermCond: Array[Spline[PieceFunction]],
                 private val cap: Array[Spline[PieceFunction]],
                 private val tempCond: Array[Spline[PieceFunction]]
                ) extends Coefficients{


    override def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): Spline[PieceFunction] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): Spline[PieceFunction] = tempCond(y + 1)

  }
  object VarXCoef{
    def apply(xDim: XDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[PieceFunction],
              buildCapCond: (Double, Double) => Spline[PieceFunction],
              buildTempCond: (Double, Double) => Spline[PieceFunction]): VarXCoef = {
      new VarXCoef(
        xDim.values.sliding(2).collect{
        case Array(x0, x1) => buildThermCond(x0, x1)
        }.toArray,
        xDim.values.sliding(2).collect{
          case Array(x0, x1) => buildCapCond(x0, x1)
        }.toArray,
        xDim.values.sliding(2).collect{
          case Array(x0, x1) => buildTempCond(x0, x1)
        }.toArray
      )
    }
  }

  import com.twitter.algebird._
  class PatchXCoef(private val thermCond: Array[Spline[PieceFunction]],
                   private val cap: Array[Spline[PieceFunction]],
                   private val tempCond: Array[Spline[PieceFunction]],
                        rangeX: Interval[Int],
                        rangeY: Interval[Int]) extends Coefficients{

    override def contains(x: Int, y: Int): Boolean = {
      rangeX.contains(x) && rangeY.contains(y)
    }

    override def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): Spline[PieceFunction] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int) = tempCond(y + 1)

  }
  object PatchXCoef{

    def apply(yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[PieceFunction],
              buildCapCond: (Double, Double) => Spline[PieceFunction],
              buildTempCond: (Double, Double) => Spline[PieceFunction],
              lX: Int, uX: Int, lY: Int, uY: Int): PatchXCoef ={

     new PatchXCoef(
       yDim.values.sliding(2).collect{
          case Array(y0, y1) => buildThermCond(y0, y1)
       }.toArray,
       yDim.values.sliding(2).collect{
         case Array(y0, y1) => buildCapCond(y0, y1)
       }.toArray,
       yDim.values.sliding(2).collect{
         case Array(y0, y1) => buildTempCond(y0, y1)
       }.toArray,
        Intersection(InclusiveLower(lX), ExclusiveUpper(uX)),
        Intersection(InclusiveLower(lY), ExclusiveUpper(uY))
     )
    }

    def apply(xDim: XDim[TypeDir], yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[PieceFunction],
              buildCapCond: (Double, Double) => Spline[PieceFunction],
              buildTempCond: (Double, Double) => Spline[PieceFunction],
              lowY: Int, uppY: Int): PatchXCoef = {
      val lowX = -1
      val uppX = xDim.colsNum + 1
      apply(yDim, buildThermCond, buildCapCond, buildTempCond, lowX, uppX, lowY, uppY)
    }
  }

  class PatchYCoef(private val thermCond: Array[Spline[PieceFunction]],
                   private val cap: Array[Spline[PieceFunction]],
                   private val tempCond: Array[Spline[PieceFunction]],
                   rangeX: Interval[Int],
                   rangeY: Interval[Int]) extends Coefficients{

    override def contains(x: Int, y: Int): Boolean = {
      rangeX.contains(x) && rangeY.contains(y)
    }

    override def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): Spline[PieceFunction] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): Spline[PieceFunction] = tempCond(x + 1)
  }
  object PatchYCoef {
    def apply(xDim: XDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[PieceFunction],
              buildCapCond: (Double, Double) => Spline[PieceFunction],
              buildTempCond: (Double, Double) => Spline[PieceFunction],
              lX: Int, uX: Int, lY: Int, uY: Int): PatchYCoef = {
      new PatchYCoef(
        xDim.values.sliding(2).collect {
          case Array(x0, x1) => buildThermCond(x0, x1)
        }.toArray,
        xDim.values.sliding(2).collect {
          case Array(x0, x1) => buildCapCond(x0, x1)
        }.toArray,
        xDim.values.sliding(2).collect {
          case Array(x0, x1) => buildTempCond(x0, x1)
        }.toArray,
        Intersection(InclusiveLower(lX), ExclusiveUpper(uX)),
        Intersection(InclusiveLower(lY), ExclusiveUpper(uY))
      )
    }

    def apply(xDim: XDim[TypeDir], yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[PieceFunction],
              buildCapCond: (Double, Double) => Spline[PieceFunction],
              buildTempCond: (Double, Double) => Spline[PieceFunction],
              lowX: Int, uppX: Int
             ): PatchYCoef = {
      val yLow = -1
      val yUpp = yDim.rowsNum + 1
      apply(xDim, buildThermCond,  buildCapCond, buildTempCond, lowX, uppX, yLow, yUpp)
    }
  }
  case class PatchedXCoef(xCoefs: VarXCoef, path: PatchXCoef) extends Coefficients{

    override def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path.thermConductivity(x, y)
      else xCoefs.thermConductivity(x, y)
    }

    override def capacity(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path.capacity(x, y)
      else xCoefs.capacity(x, y)
    }

    override def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path.tempConductivity(x, y)
      else xCoefs.tempConductivity(x, y)
    }

    override def apply(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path(x, y) else xCoefs(x, y)
    }
  }

  case class PatchedYCoef(yCoefs: VarYCoef, path: PatchYCoef) extends Coefficients{

    override def thermConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path.thermConductivity(x, y)
      else yCoefs.thermConductivity(x, y)
    }

    override def capacity(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path.capacity(x, y)
      else yCoefs.capacity(x, y)
    }

    override def tempConductivity(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path.tempConductivity(x, y)
      else yCoefs.tempConductivity(x, y)
    }

    override def apply(x: Int, y: Int): Spline[PieceFunction] = {
      if (path.contains(x, y)) path(x, y) else yCoefs(x, y)
    }
  }

  def getRadialMidPoint(r0: Double, r1: Double, r2: Double,
                        t0: Double, t1: Double): Double = {
    assert(r2 - r1 == r1 - r0)
    val rAtHalf0 = (r0 + r1) / 2.0
    val rAtHalf1 = (r1 + r2) / 2.0
    (t0 * rAtHalf0 + t1 * rAtHalf1) / (rAtHalf0 + rAtHalf1)
  }

  def getRadialMidPoint(r0: Double, r1: Double, r2: Double,
                        k0: Double, k1: Double,
                        t0: Double, t1: Double): Double = {
    val rAtHalf0 = (r0 + r1) / 2.0
    val rAtHalf1 = (r1 + r2) / 2.0
    val a = rAtHalf0 * k0 * (r1 - r0)
    val b = rAtHalf1 * k1 * (r2 - r1)
    (a * t0 + b * t1) / (a + b)
  }

  def discHeatFlow(t0: Double, t1: Double,
                   r0: Double, r1: Double,
                   k: Double): Double = {
    val rAtHalf = (r0 + r1) / 2.0
    k * rAtHalf * (t0 - t1) / (r1 - r0) * math.Pi * 2.0
  }



  def radialHeatFlow(t0: Double, t1: Double,
                     r0: Double, r1: Double,
                     k: Double): Double = {
    val resist = math.log(r1 / r0) / (math.Pi * 2.0 * k)
    (t0 - t1) / resist
  }

  def discTWithFlow(heatFlow: Double,
                    t0: Double,
                    r0: Double,
                    r1: Double,
                    k: Double): Double = {
    val rAtHalf = (r0 + r1) / 2.0
    val coef = k * rAtHalf / (r1 - r0) * math.Pi * 2.0
    t0 - heatFlow / coef
  }

  case class Shape(rows: Int, cols: Int)


}
