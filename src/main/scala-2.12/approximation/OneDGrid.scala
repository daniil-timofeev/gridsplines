package approximation

import java.text.NumberFormat
import java.time.LocalDateTime
import java.util.Locale
import ammonite.ops._
import passion._
import piecewise._
import OneDGrid._


/**
  *
  */
case class OneDGrid[Dir <: GridDir](leftX: Double,
                                    rangeX: Array[Double],
                                    rightX: Double,
                                    conductivities: Array[Spline[PieceFunction]], sigma: Double)(implicit dir: Dir){

  def this(leftX: Double, rangeX: Array[Double], rightX: Double,
           conductivity: Spline[PieceFunction], sigma: Double)(implicit  dir: Dir){
    this(leftX, rangeX, rightX, Array.fill(rangeX.length)(conductivity), sigma)(dir)
  }


  val grid = Array.fill(rangeX.length)(4.0)
  private val predict = Array.fill(rangeX.length)(4.0)
  private val result = grid.clone()
  private val passion = Array.fill(rangeX.length)(new Array[Double](2))
  private val preDef: Array[Array[Double]] = dir.preDef(leftX, rangeX, rightX, sigma)

  def step(time: Double, leftT: Double, rightT: Double) = {
    iteration(time, leftT, grid, rightT, predict, preDef, conductivities, passion, result)
    var i = 0
    while(i != predict.size){
      predict.update(i, arraygrid.aVal(grid(i), result(i)))
      grid.update(i, result(i))
      i += 1
    }
  }

  def writeGrid(at: LocalDateTime, path: Path): Unit = {

    val format2 = NumberFormat.getNumberInstance(Locale.ROOT)
    format2.setMaximumFractionDigits(2)
    val format3 = NumberFormat.getNumberInstance(Locale.ROOT)
    format3.setMaximumFractionDigits(3)

    mkdir! path

    val rI = rangeX.iterator
    val rG = grid.iterator

    val filePath: Path = path/s"grid ${at.getMonth} ${at.getDayOfMonth}d ${at.getHour}h ${at.getYear}.dat"
    val iter =
    Iterator.single("# One dimension grid output results\r\n# Coords \t Temperatures\r\n") ++
    Iterator.tabulate(rangeX.length){i =>
      s"${format2.format(rangeX(i))}\t${format3.format(grid(i))}\r\n"
    }
    write.over(filePath, iter)
  }
}

object OneDGrid{

  //TODO add "implicit not found" annotation
  def apply[Dir <: GridDir](leftX: Double, rangeX: Array[Double], rightX: Double,
  conductivity: Spline[PieceFunction], sigma: Double)(implicit  dir: Dir) = {
    val array: Array[Spline[PieceFunction]] = Array.fill(rangeX.length)(conductivity)
    new OneDGrid[Dir](leftX, rangeX, rightX, array, sigma)
  }
}
