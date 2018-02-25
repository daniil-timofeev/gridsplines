package approximation

import java.io.{BufferedWriter, IOException}

import approximation.TwoDGrid.Bounds._
import approximation.TwoDGrid.{Coefficients, _}
import approximation.XDim.RowIterator
import approximation.YDim.ColumnIterator
import piecewise.Spline.PieceFunFactory
import piecewise.{PieceFunction, Spline}

import scala.math.abs

class TwoDGrid[XType <: TypeDir, YType <: TypeDir, P <: PieceFunction](
        val x: XDim[XType], val y: YDim[YType],
        val bounds: Bounds,
        val coefs: Coefficients[P]
){

  val grid = TwoDGrid.makeArray(x, y)

  /** Update temperature, depending on x coordinate
    * @param f function,
    *          which return temperature of the grid row with `y` coordinate,
    *          given at argument
    * */
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

  /** Update temperature, depending on y coordinate
    * @param f function,
    *          which return temperature of the grid column with `x` coordinate,
    *          given at argument
    * */
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
      var c0 = bounds.left match {
        case temp: Temperature => {
          val t1 = temp.get(row)
          x.first(time, t1, t2, t3, t,
            coefs(-1, row),
            coefs(col, row)
          )
        }
        case heatFlow: HeatFlow => {
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
        case tempBound: Temperature => {
          t3 = tempBound.get(row)
          x.last(col, time, t2, t3, t, c0,
            coefs(col, row))
        }

        case heatFlowBound: HeatFlow => {
          x.lastHeatFlow(col, time, heatFlowBound.get(row), t2, t3, t,
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
      var t2 = grid(i)
      var t3 = grid(i + colsNum)
      var t = grid.res(i)
      var c0 = bounds.upp match {
        case tempBound: Temperature => {
          val t1 = tempBound.get(col)
          y.first(time, t1, t2, t3, t,
            coefs(col, -1),
            coefs(col, row))
        }
        case heatFlowBound: HeatFlow => {
          y.firstHeatFlow(time, heatFlowBound.get(col), t2, t3, t,
            coefs.tempConductivity(col, row),
            coefs.capacity(col, row)
          )
        }
      }

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
        case temp: Temperature => {
          t3 = bounds.low.get(col)
          y.last(row, time, t2, t3, t, c0,
            coefs(col, row))
        }
        case heatFlow: HeatFlow => {
          val t1 = grid(i - 1)
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

  def col(colNum: Int): Array[Double] = {
    val array = new Array[Double](y.rowsNum)
    col(colNum, array)
    array
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

  def row(rowNum: Int): Array[Double] = {
    var array = new Array[Double](x.colsNum)
    row(rowNum, array)
    array
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

  def rowCoefs(rowNum: Int): Array[AlwaysDefinedSpline[P]] = {
    val result = new Array[AlwaysDefinedSpline[P]](x.colsNum)
    var xInd = 0
    while (xInd != x.colsNum) {
      result.update(xInd, coefs(xInd, rowNum))
      xInd += 1
    }
    result
  }

  def colCoefs(colNum: Int): Array[AlwaysDefinedSpline[P]] = {
    val result = new Array[AlwaysDefinedSpline[P]](y.rowsNum)
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
        bounds.upp.bound match {
          case base: Dense =>  upper(0, base.array)
          case one: Sparse => one.update(upperAverage)
        }
      }
      case Lower => {
        bounds.low.bound match {
          case base: Dense => lower(0, base.array)
          case one: Sparse => one.update(lowerAverage)
        }
      }
      case Right => {
        bounds.right.bound match {
          case base: Dense => right(0, base.array)
          case one: Sparse => one.update(rightAverage)
        }
      }
      case Left => {
        bounds.left.bound match {
          case base: Dense => left(0, base.array)
          case one: Sparse => one.update(leftAverage)
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
object TwoDGrid {

  def makeArray[XType <: TypeDir,
  YType <: TypeDir](
                     xDim: XDim[XType],
                     yDim: YDim[YType]): Grid = {
    val grid, predict, result = new Array[Double](xDim.colsNum * yDim.rowsNum)
    new Grid(grid, predict, result)
  }

  class Grid private[TwoDGrid](val grid: Array[Double],
                               val predict: Array[Double],
                               val result: Array[Double]) {
    def get(i: Int): Double = grid(i)

    def res(i: Int): Double = result(i)

    def put(i: Int, value: Double): Unit = result.update(i, value)

    def getP(i: Int): Double = predict(i)

    def apply(i: Int): Double = predict(i)

    def update(): Unit = {
      var i = 0
      while (i < grid.length) {
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
      while (i < grid.length) {
        grid.update(i, v)
        predict.update(i, v)
        i += 1
      }
    }

    def *=(i: Int, value: Double): Unit = {
      grid.update(i, value)
    }

    /** Переопределяет предположение о темепературе на следующем шаге
      *
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

  trait Count

  case object One extends Count

  type One = One.type

  case object Many extends Count

  type Many = Many.type

  trait BoundT

  case object Temp extends BoundT

  type Temp = Temp.type

  case object Flow extends BoundT

  type Flow = Flow.type


  private def boundBuilder[C <: Count, B <: BoundT](side: BoundSide, c: C, b: B)(
      implicit xDim: XDim[_], yDim: YDim[_]
    ): Bound = {
      (c, b) match{
        case (One, Temp) => Temperature()
        case (Many, Temp) => Temperature(side, xDim, yDim)
        case (One, Flow) => HeatFlow()
        case (Many, Flow) => HeatFlow(side, xDim, yDim)
      }
  }

  def apply[X <: TypeDir, Y <: TypeDir,
            P <: PieceFunction,
            C1 <: Count, B1 <: BoundT,
            C2 <: Count, B2 <: BoundT,
            C3 <: Count, B3 <: BoundT,
            C4 <: Count, B4 <: BoundT](
       xDim: XDim[X],
       yDim: YDim[Y])
      (upCount: C1, upBound: B1)
      (lowCount: C2, lowBound: B2)
      (rightCount: C3, rightBound: B3)
      (leftCount: C4, leftBound: B4)(coefs: Coefficients[P]): TwoDGrid[X, Y, P] = {
    implicit val x = xDim
    implicit val y = yDim
    val bounds = Bounds.apply(
      boundBuilder(Upper, upCount, upBound),
      boundBuilder(Lower, lowCount, lowBound),
      boundBuilder(Right, rightCount, rightBound),
      boundBuilder(Left, leftCount, leftBound)
    )
    new TwoDGrid(x, y, bounds, coefs)
  }


  abstract class BoundRep{
    /** get bound value at `i` index*/
    def get(i: Int): Double
    /** update bound values with `values` array */
    def update(values: Array[Double]): Unit
    /** update bound all bound values with `value`*/
    def update(value: Double): Unit
    /** alias for `update(values: Array[Double]): Unit`*/
    def *=(values: Array[Double]): Unit = update(values)
    /** alias for `update(value: Double): Unit` */
    def *=(value: Double): Unit = update(value)

    def updateByIndex(f: (Double, Double) => Double): Unit
    def updateByCoord(f: (Int, Double) => Double): Unit
  }

  class Sparse(private var value: Double = 0.0) extends BoundRep{
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

    override def updateByIndex(f: (Double, Double) => Double): Unit = {
      update(f(0.0, value))
    }

    override def updateByCoord(f: (Int, Double) => Double): Unit = {
      update(f(0, value))
    }
  }

  class Dense private(range: Array[Double]) extends BoundRep{

    def this(side: BoundSide, xDim: XDim[_], yDim: YDim[_]){
      this(side.range(xDim, yDim))
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

    /** Update values, depends on coordinate
      *
      * @param f function `(coordinate, oldValue) => newValue`
      */
    override def updateByIndex(f: (Double, Double) => Double): Unit = {
      var i = 0
      while (i != values.length) {
        values.update(i, f(range(i), values(i)))
        i += 1
      }
    }

    /** Update values, depends on index
      *
      * @param f function `(index, oldValue) => newValue`
      */
    override def updateByCoord(f: (Int, Double) => Double): Unit = {
      var i = 0
      while (i != values.length) {
        values.update(i, f(i, values(i)))
        i += 1
      }
    }

    def array: Array[Double] = values
  }

  abstract class Bound{
    val bound: BoundRep

    /** get bound value at `i` index*/
    def get(i: Int): Double = bound.get(i)

    def updateByCoord(f: (Double, Double) => Double) = bound.updateByIndex(f)

    def updateByIndex(f: (Int, Double) => Double) = bound.updateByCoord(f)
    /** update bound all bound values with `value`*/
    def update(value: Double) = bound.update(value)
    /** update bound values with `values` array */
    def update(values: Array[Double]) = bound.update(values)

    /** alias for `update(values: Array[Double]): Unit`*/
    def *=(values: Array[Double]): Unit = update(values)
    /** alias for `update(value: Double): Unit` */
    def *=(value: Double): Unit = update(value)
  }

  case class HeatFlow(bound: BoundRep) extends Bound{

    def this(side: BoundSide, xDim: XDim[_], yDim: YDim[_]){
      this(new Dense(side, xDim, yDim))
    }

  }
  object HeatFlow{

    def apply(side: BoundSide, xDim: XDim[_], yDim: YDim[_]): HeatFlow = {
      new HeatFlow(side, xDim, yDim)
    }

    def apply(value: Double): HeatFlow = {
      new HeatFlow(new Sparse(value))
    }

    def apply(): HeatFlow = {
      new HeatFlow(new Sparse())
    }
  }

  case class Temperature(bound: BoundRep) extends Bound {

    def this(side: BoundSide, xDim: XDim[_], yDim: YDim[_]){
      this(new Dense(side, xDim, yDim))
    }
  }
  object Temperature{

    def apply(side: BoundSide, xDim: XDim[_], yDim: YDim[_]): Temperature = {
      new Temperature(side, xDim, yDim)
    }

    def apply(value: Double): Temperature = {
      new Temperature(new Sparse(value))
    }

    def apply(): Temperature = {
      new Temperature(new Sparse(0.0))
    }
  }

  case class Bounds(upp: Bound,
                    low: Bound,
                    right: Bound,
                    left: Bound){
    def *= (value: Double) = {
      upp.update(value)
      low.update(value)
      right.update(value)
      left.update(value)
    }
  }
  object Bounds {


    abstract class BoundSide {
      def size(x: XDim[_], y: YDim[_]): Int

      def range(x: XDim[_], y: YDim[_]): Array[Double]
    }

    case object Upper extends BoundSide {
      def size(x: XDim[_], y: YDim[_]): Int = {
        x.colsNum
      }

      def range(x: XDim[_], y: YDim[_]): Array[Double] = x.range
    }

    case object Lower extends BoundSide {
      def size(x: XDim[_], y: YDim[_]): Int = {
        x.colsNum
      }

      def range(x: XDim[_], y: YDim[_]): Array[Double] = x.range
    }

    case object Right extends BoundSide {
      def size(x: XDim[_], y: YDim[_]): Int = {
        y.rowsNum
      }

      def range(x: XDim[_], y: YDim[_]): Array[Double] = y.range
    }

    case object Left extends BoundSide {
      override def size(x: XDim[_], y: YDim[_]): Int = {
        y.rowsNum
      }

      def range(x: XDim[_], y: YDim[_]): Array[Double] = y.range
    }
  }
  abstract class Coefficients[P <: PieceFunction] {
    def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P]
    def capacity(x: Int, y: Int): AlwaysDefinedSpline[P]
    def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P]
    def apply(x: Int, y: Int): AlwaysDefinedSpline[P]
    def contains(x: Int, y: Int): Boolean = true
  }

  class ConstantCoef[P <: PieceFunction](
    private val thermCond: AlwaysDefinedSpline[P],
    private val cap: AlwaysDefinedSpline[P],
    private val tempCond: AlwaysDefinedSpline[P]) extends Coefficients[P]{

    def this(thermCond: Spline[P], cap: Spline[P], tempCond: Spline[P]){
      this(
        new AlwaysDefinedSpline(thermCond),
        new AlwaysDefinedSpline(cap),
        new AlwaysDefinedSpline(tempCond)
      )
    }

    def this(thermCond: Spline[P], cap: Spline[P])(
      implicit P: PieceFunFactory[P]){
      this(
        new AlwaysDefinedSpline(thermCond),
        new AlwaysDefinedSpline(cap),
        new AlwaysDefinedSpline({thermCond / cap}.get))
    }

    def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = thermCond
    def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = cap
    def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = tempCond
    def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = tempCond
    val contains = true
  }

  class VarYCoef[P <: PieceFunction](
                 private val thermCond: Array[AlwaysDefinedSpline[P]],
                 private val cap: Array[AlwaysDefinedSpline[P]],
                 private val tempCond: Array[AlwaysDefinedSpline[P]]
                ) extends Coefficients[P]{

    def this(thermCond: Array[Spline[P]],
             cap: Array[Spline[P]],
             tempCond: Array[Spline[P]]
            ){
      this(
        thermCond.map(c => new AlwaysDefinedSpline(c)),
        cap.map(c => new AlwaysDefinedSpline(c)),
        tempCond.map(c => new AlwaysDefinedSpline(c))
      )
      }

    override def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = tempCond(y + 1)
  }

  object VarYCoef{
    def apply[P <: PieceFunction](yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[P],
              buildCapCond: (Double, Double) => Spline[P],
              buildTempCond: (Double, Double) => Spline[P]
             ): VarYCoef[P] = {
      new VarYCoef[P](
        yDim.values.sliding(2).collect{
          case Array(y0, y1) => new AlwaysDefinedSpline(buildThermCond(y0, y1))
        }.toArray,
        yDim.values.sliding(2).collect{
          case Array(y0, y1) => new AlwaysDefinedSpline(buildCapCond(y0, y1))
        }.toArray,
        yDim.values.sliding(2).collect{
          case Array(y0, y1) => new AlwaysDefinedSpline(buildTempCond(y0, y1))
        }.toArray
      )
    }
  }

  class VarXCoef[P <: PieceFunction](
                 private val thermCond: Array[AlwaysDefinedSpline[P]],
                 private val cap: Array[AlwaysDefinedSpline[P]],
                 private val tempCond: Array[AlwaysDefinedSpline[P]]
                ) extends Coefficients[P]{

     def this(thermCond: Array[Spline[P]],
              cap: Array[Spline[P]],
              tempCond: Array[Spline[P]]){
       this(
         thermCond.map(c => new AlwaysDefinedSpline(c)),
         cap.map(c => new AlwaysDefinedSpline(c)),
         tempCond.map(c => new AlwaysDefinedSpline(c))
       )
     }
    override def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = tempCond(y + 1)

  }
  object VarXCoef{
    def apply[P <: PieceFunction](xDim: XDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[P],
              buildCapCond: (Double, Double) => Spline[P],
              buildTempCond: (Double, Double) => Spline[P]): VarXCoef[P] = {
      new VarXCoef(
        xDim.values.sliding(2).collect{
        case Array(x0, x1) => new AlwaysDefinedSpline(buildThermCond(x0, x1))
        }.toArray,
        xDim.values.sliding(2).collect{
          case Array(x0, x1) => new AlwaysDefinedSpline(buildCapCond(x0, x1))
        }.toArray,
        xDim.values.sliding(2).collect{
          case Array(x0, x1) => new AlwaysDefinedSpline(buildTempCond(x0, x1))
        }.toArray
      )
    }
  }

  import com.twitter.algebird._
  class PatchXCoef[P <: PieceFunction](
                   private val thermCond: Array[AlwaysDefinedSpline[P]],
                   private val cap: Array[AlwaysDefinedSpline[P]],
                   private val tempCond: Array[AlwaysDefinedSpline[P]],
                        rangeX: Interval[Int],
                        rangeY: Interval[Int]) extends Coefficients[P]{

    def this(thermCond: Array[Spline[P]],
             cap: Array[Spline[P]],
             tempCond: Array[Spline[P]],
             rangeX: Interval[Int],
             rangeY: Interval[Int]
            ){
      this(
        thermCond.map(c => new AlwaysDefinedSpline(c)),
        cap.map(c => new AlwaysDefinedSpline(c)),
        tempCond.map(c => new AlwaysDefinedSpline(c)),
        rangeX, rangeY
      )
    }

    override def contains(x: Int, y: Int): Boolean = {
      rangeX.contains(x) && rangeY.contains(y)
    }

    override def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = tempCond(y + 1)

  }
  object PatchXCoef{

    def apply[P <: PieceFunction](xDim: XDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[P],
              buildCapCond: (Double, Double) => Spline[P],
              buildTempCond: (Double, Double) => Spline[P],
              lX: Int, uX: Int, lY: Int, uY: Int): PatchXCoef[P] ={

     new PatchXCoef(
       xDim.values.sliding(2).collect{
          case Array(x0, x1) => new AlwaysDefinedSpline(buildThermCond(x0, x1))
       }.toArray,
       xDim.values.sliding(2).collect{
         case Array(x0, x1) => new AlwaysDefinedSpline(buildCapCond(x0, x1))
       }.toArray,
       xDim.values.sliding(2).collect{
         case Array(x0, x1) => new AlwaysDefinedSpline(buildTempCond(x0, x1))
       }.toArray,
        Intersection(InclusiveLower(lX), ExclusiveUpper(uX)),
        Intersection(InclusiveLower(lY), ExclusiveUpper(uY))
     )
    }

    def apply[P <: PieceFunction](
              xDim: XDim[TypeDir], yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[P],
              buildCapCond: (Double, Double) => Spline[P],
              buildTempCond: (Double, Double) => Spline[P],
              lowY: Int, uppY: Int): PatchXCoef[P] = {
      val lowX = -1
      val uppX = xDim.colsNum + 1
      apply(xDim, buildThermCond, buildCapCond, buildTempCond, lowX, uppX, lowY, uppY)
    }
  }

  class PatchYCoef[P <: PieceFunction](
                   private val thermCond: Array[AlwaysDefinedSpline[P]],
                   private val cap: Array[AlwaysDefinedSpline[P]],
                   private val tempCond: Array[AlwaysDefinedSpline[P]],
                   rangeX: Interval[Int],
                   rangeY: Interval[Int]) extends Coefficients[P]{

    def this(thermCond: Array[Spline[P]],
             cap: Array[Spline[P]],
             tempCond: Array[Spline[P]],
             rangeX: Interval[Int],
             rangeY: Interval[Int]
            ){
      this(
        thermCond.map(c => new AlwaysDefinedSpline(c)),
        cap.map(c => new AlwaysDefinedSpline(c)),
        tempCond.map(c => new AlwaysDefinedSpline(c)),
        rangeX, rangeY
      )
    }

    override def contains(x: Int, y: Int): Boolean = {
      rangeX.contains(x) && rangeY.contains(y)
    }

    override def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      thermCond(y + 1)
    }

    override def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      cap(y + 1)
    }

    override def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      tempCond(y + 1)
    }

    def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = tempCond(x + 1)
  }
  object PatchYCoef {
    def apply[P <: PieceFunction](yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[P],
              buildCapCond: (Double, Double) => Spline[P],
              buildTempCond: (Double, Double) => Spline[P],
              lX: Int, uX: Int, lY: Int, uY: Int): PatchYCoef[P] = {
      new PatchYCoef(
        yDim.values.sliding(2).collect {
          case Array(x0, x1) => new AlwaysDefinedSpline(buildThermCond(x0, x1))
        }.toArray,
        yDim.values.sliding(2).collect {
          case Array(x0, x1) => new AlwaysDefinedSpline(buildCapCond(x0, x1))
        }.toArray,
        yDim.values.sliding(2).collect {
          case Array(x0, x1) => new AlwaysDefinedSpline(buildTempCond(x0, x1))
        }.toArray,
        Intersection(InclusiveLower(lX), ExclusiveUpper(uX)),
        Intersection(InclusiveLower(lY), ExclusiveUpper(uY))
      )
    }

    def apply[P <: PieceFunction](
              xDim: XDim[TypeDir], yDim: YDim[TypeDir],
              buildThermCond: (Double, Double) => Spline[P],
              buildCapCond: (Double, Double) => Spline[P],
              buildTempCond: (Double, Double) => Spline[P],
              lowX: Int, uppX: Int
             ): PatchYCoef[P] = {
      val yLow = -1
      val yUpp = yDim.rowsNum + 1
      apply(yDim, buildThermCond,  buildCapCond, buildTempCond, lowX, uppX, yLow, yUpp)
    }
  }
  case class PatchedXCoef[P <: PieceFunction](xCoefs: VarXCoef[P],
                                              path: PatchXCoef[P])
    extends Coefficients[P]{

    override def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path.thermConductivity(x, y)
      else xCoefs.thermConductivity(x, y)
    }

    override def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path.capacity(x, y)
      else xCoefs.capacity(x, y)
    }

    override def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path.tempConductivity(x, y)
      else xCoefs.tempConductivity(x, y)
    }

    override def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path(x, y) else xCoefs(x, y)
    }
  }

  case class PatchedYCoef[P <: PieceFunction](yCoefs: VarYCoef[P],
                                              path: PatchYCoef[P])
    extends Coefficients[P]{

    override def thermConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path.thermConductivity(x, y)
      else yCoefs.thermConductivity(x, y)
    }

    override def capacity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path.capacity(x, y)
      else yCoefs.capacity(x, y)
    }

    override def tempConductivity(x: Int, y: Int): AlwaysDefinedSpline[P] = {
      if (path.contains(x, y)) path.tempConductivity(x, y)
      else yCoefs.tempConductivity(x, y)
    }

    override def apply(x: Int, y: Int): AlwaysDefinedSpline[P] = {
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
