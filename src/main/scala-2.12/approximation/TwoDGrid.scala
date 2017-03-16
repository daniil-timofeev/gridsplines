package approximation

import approximation.passion.iteration
import scala.math._
import piecewise.{PieceFunction, Spline}
import com.twitter.algebird.matrix._
import org.slf4j._
//TODO make all arrays immutable (scala 2.13)
/**
  *
  */
case class TwoDGrid[D1 <: GridDir, D2 <: GridDir](
                                                   leftX: Array[Double],
                                                   rangeX: Array[Array[Double]],
                                                   rightX: Array[Double],
                                                   xBeginAt: Array[Int],
                                                   leftY: Array[Double],
                                                   rangeY: Array[Double],
                                                   rightY: Array[Double],
                                                   conductivities: Array[Spline[PieceFunction]],
                                                   sigma: Double
                                                 )(implicit val dir1: D1, implicit val dir2: D2) {

  private val logger = LoggerFactory.getLogger(getClass)
  logger.info("Creating two dimensional grid...")
  protected val grid = Array.fill(rangeY.length * rangeX.map(_.length).max)(4.0)
  logger.info(s"Innitial grid size ${rangeY.length * rangeX.map(_.length).max}")
  protected val afterFirstIteration = grid.clone()
  protected val predicted = grid.clone()
  protected val result = grid.clone()
  logger.trace("Copies of the grid was successfuly created")
  assert(grid.length == conductivities.length)
  protected def index(row: Int, col: Int): Int = {
    row * col + col
  }
  logger.info("Acessing for each Y (row)...")
  logger.info("indexes:")

  private val xLoop: Array[Int] =
    {(rangeX.view zip xBeginAt).zipWithIndex flatMap{(zipped: ((Array[Double], Int), Int)) =>{
        val ((x: Array[Double], x0: Int), y: Int) = zipped //for each y
        x.indices.map((xI: Int) => {
          val ind = index(y, xI + x0)
          logger.info(s"((y:${y}, x:${xI + x0}) -> ${ind});")
          ind}) // calculate index with the same y and existing x
    }}}.toArray

  private val yLoop: Array[Int] =
    {rangeX.maxBy(_.length).indices flatMap{x =>{ //for each x
      rangeY.indices.view collect{
        { // for each y find where x exist
          case y if rangeX(y).indices.contains(x - xBeginAt(y)) => {
            val ind = index(y, x)
            logger.info(s"((y:${y}, x:${x}) -> ${ind});")
            ind
          }
        }
      }}
    }}.toArray
  logger.info("---")


  private val xLengths = rangeX.map((xRow: Array[Double]) => xRow.length)
  private val yLengths: Array[Int] = {
    rangeX.maxBy(_.length).indices.map{(x: Int) =>{ //for each x
      rangeY.indices.count{y =>{
        // if row must contain non null value count it
        rangeX(y).indices.contains(x - xBeginAt(y))
      }}
    }}
  }.toArray

  private val xPreDef: Array[Array[Double]] =
  (leftX, rangeX, rightX).zipped
    .map((lX: Double, ranX: Array[Double], rX: Double) => dir1.preDef(lX, ranX, rX, sigma)).reduce(_ ++ _)

  private val whereYHas = rangeX.maxBy(_.length).indices
    .map{{x: Int =>{
      rangeY.zipWithIndex collect{
        {
          case (yL, yI) if rangeX(yI).indices.contains(x - xBeginAt(yI)) => yL
        }
      }
    }
    }}

  private val yPreDef: Array[Array[Double]] =
    (leftY, whereYHas, rightY).zipped
      .map{(lY: Double, midY: Array[Double], rY: Double) => {
        dir2.preDef(lY, midY, rY, sigma)
      }}
      .reduce(_ ++ _)

  private val toPassion = Array.fill(max(rangeX.map(_.length).max, whereYHas.map(_.length).max))(new Array[Double](2))
  private val localResult = new Array[Double](max(rangeX.map(_.length).max, whereYHas.map(_.length).max))

  def step(time: Double,
           leftTX: Array[Double], rightTX: Array[Double],
           leftTY: Array[Double], rightTY: Array[Double]): Unit = {

    iteration(time, xLoop, xLengths,
      leftTX, grid, predicted, localResult, afterFirstIteration, rightTX,
      xPreDef, conductivities, toPassion)
    iteration(time, yLoop, yLengths,
      leftTY, afterFirstIteration, predicted, localResult, result, rightTY,
      yPreDef, conductivities, toPassion)

    var i = 0
    while(i != grid.length){
      result.update(i, arraygrid.aVal(grid(i), result(i)))
      grid.update(i, result(i))
      i += 1
    }
  }

}
object TwoDGrid{

  /** Get column of grid at index `x`
    *
    * @param grid grid with rows and columns
    * @param colArray mutable column array, for avoid its allocation
    * @param x column index
    * @return
    */
  def gridColumn(grid: Array[Array[Double]], colArray: Array[Double], x: Int): Array[Double] = {
    var c = 0
    while(c < colArray.length){
      colArray.update(c, grid(c)(x))
      c += 1
    }
    colArray
  }

  def gridColumn(colArray: Array[Double], grid: Array[Array[Double]], x: Int): Array[Array[Double]] = {
    var c = 0
    while(c < colArray.length){
      grid(c).update(x, colArray(c))
      c += 1
    }
    grid
  }
}
