package approximation

import java.text.NumberFormat
import java.util.Locale

import approximation.TwoDGrid.{Dir, Side, X, Y}
import approximation.passion.iteration

import scala.math._
import piecewise.{PieceFunction, Spline}

import scala.annotation.tailrec
import org.slf4j._

//TODO make all arrays immutable (scala 2.13)
/**
  *
  */
 class TwoDGrid[XDir <: TypeDir, YDir <: TypeDir](leftX: Array[Double],
                                                  rangeX: Array[Double],
                                                  rightX: Array[Double],
                                                  xBeginAt: Array[Int],
                                                  xEndAt: Array[Int],
                                                  leftY: Array[Double],
                                                  rangeY: Array[Double],
                                                  rightY: Array[Double],
                                                  conductivities: Array[Spline[PieceFunction]],
                                                  weight: Double
                                                 )(implicit val xDir: XDir, implicit val yDir: YDir) {

  assert(rangeX.length == rangeY.length)
  assert(xBeginAt.length == rangeX.length)

  private def tallestXRange = (xBeginAt, xEndAt).zipped.map((x0, xn) => x0 to xn by 1).maxBy(_.length)

  private def tallestXLength = tallestXRange.length

  private def isDeterminedAt(colI: Int, rowI: Int): Boolean = xBeginAt(colI) < rowI && rowI < xEndAt(colI)

  private val logger = LoggerFactory.getLogger(getClass)
  logger.info("Creating two dimensional grid...")
  protected val grid = Array.fill(rangeY.length * tallestXLength)(4.0)
  logger.info(s"Innitial grid size ${rangeY.length * tallestXLength}")
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

  private val xLoop: Array[Int] = {
    (xBeginAt zip xEndAt).zipWithIndex flatMap {(zipped: ((Int, Int), Int)) =>
      val ((x0: Int, xn: Int), y: Int) = zipped //for each y
      x0.to(xn, 1).map{(xI: Int) =>
        val ind = index(y, xI + x0)
        logger.info(s"((y:${y}, x:${xI + x0}) -> ${ind});")
        ind
      } // calculate index with the same y and existing x
    }
  }.toArray

  private val yLoop: Array[Int] = {
    tallestXRange.indices flatMap { x =>
      //for each x
      rangeY.indices.view collect {
        {
          // for each y find where x exist
          case y if (xBeginAt(y) to xEndAt(y) by 1).indices.contains(x - xBeginAt(y)) => {
            val ind = index(y, x)
            logger.info(s"((y:${y}, x:${x}) -> ${ind});")
            ind
          }
        }
      }
    }
  }.toArray
  logger.info("---")


  private val xLengths = (xBeginAt, xEndAt).zipped.map((x0, xn) => (x0 to xn by 1).length)
  //remake
  private val yLengths: Array[Int] = {
    rangeX.indices.map { (x: Int) =>
      //for each x
      rangeY.indices.count (y => isDeterminedAt(y, x)
      )
    }
  }.toArray

  private val xPreDef: Array[Array[Double]] = {

    val ranges: Array[Array[Double]] = (xBeginAt, xEndAt).zipped.map((x0, xn) => TwoDGrid.sliceX(x0, rangeX, xn))


    (leftX, ranges, rightX).zipped
      .map((lX: Double, ranX: Array[Double], rX: Double) => xDir.preDef(lX, ranX, rX, weight)).reduce(_ ++ _)
  }

  private def whereYHas = rangeX.indices
    .map { x: Int =>
       rangeY.zipWithIndex collect {
         case (yL, yI) if isDeterminedAt(yI, x) => yL
       }
    }

  private val yPreDef: Array[Array[Double]] =
    (leftY, whereYHas, rightY).zipped
      .map {(lY: Double, midY: Array[Double], rY: Double) =>
        yDir.preDef(lY, midY, rY, weight)
      }
      .reduce(_ ++ _)

  private val toPassion = Array.fill(max(rangeX.length, whereYHas.map(_.length).max))(new Array[Double](2))
  private val localResult = toPassion.map(_(0))

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
    while (i != grid.length) {
      result.update(i, arraygrid.aVal(grid(i), result(i)))
      grid.update(i, result(i))
      i += 1
    }
  }

  import java.nio.file._
  def write(columnSeparator: String, dir: Path,
            fileName: String, fileExtension: String,
            commentSign: String, comments: List[String]) = {
    import java.nio.file.Files
    val form = NumberFormat.getNumberInstance(Locale.ROOT)
    form.setMaximumFractionDigits(3)

    val file = dir.resolve(fileName + fileExtension)

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits._
    Future {
      val writer = Files.newBufferedWriter(file, StandardOpenOption.TRUNCATE_EXISTING)
      try {
        if (comments.nonEmpty) {
          comments.foreach(comment => {
            writer.write(commentSign + comment)
            writer.newLine()
          })
        }
        @tailrec def writeRows(lengths: List[Int], indent: List[Int], grid: List[Double]): Unit = {
          if (lengths.nonEmpty) {
            val result: String = {
              List.fill(indent.head)(Double.NaN) ++
                grid.take(lengths.head + indent.head) ++
                List.fill(tallestXLength - lengths.head - indent.head)(Double.NaN)
            }.map(form.format)
             .reduce(_ + columnSeparator + _)
            writer.write(result)
            writer.newLine()
            writeRows(lengths.tail, indent.tail, grid.drop(tallestXLength))
          }
        }
      }
      finally writer.close()
    }

  }




  private[approximation] def overlapBound[D <: Dir, S <: Side](width: Int)(implicit dir: D, side: S): TypeDir = {
    import TwoDGrid._
    dir match{
      case x: X => {
        side match {
          case left: Left => ???
          case right: Right => ???
        }
      }
      case y: Y => {
        side match{
          case left: Left => ???
          case right: Right =>  ???
        }
      }
    }
  }

}
object TwoDGrid{

  def apply[xDir <: TypeDir, yDir <: TypeDir](leftX: Array[Double], coordX: Array[Double], rightX: Array[Double],
                                              leftY: Array[Double], coordY: Array[Double], rightY: Array[Double],
                                              conductivities: Array[Spline[PieceFunction]], weight: Double)
                                             (implicit dir1: xDir, dir2: yDir): TwoDGrid[xDir, yDir] = {

    def xStartAt = leftX.map{x =>
     coordX.indexWhere(coord => coord > x)
    }

    def xEndsAt = rightX.map{x =>
      coordX.indexWhere(coord => x >= coord) - 1
    }

    new TwoDGrid[xDir, yDir](leftX, coordX, rightX, xStartAt, xEndsAt, leftY, coordY, rightY, conductivities, weight)
    }

  def sliceX(beginAt: Int, coordX: Array[Double], endAt: Int): Array[Double] = {
    coordX.slice(beginAt, endAt + 1)
  }

  abstract class Dir

  class X extends Dir
  class Y extends Dir

  abstract class Side
  class Left extends Side
  class Right extends Side


}

