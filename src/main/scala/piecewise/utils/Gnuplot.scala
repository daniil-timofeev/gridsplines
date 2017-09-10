package piecewise.utils

import com.twitter.algebird._
import com.twitter.algebird.Interval._
import piecewise.{PieceFunction, Spline}
import java.nio.file._

/**
  * Created by Даниил on 10.04.2017.
  */
object Gnuplot {

  implicit val spline2Gnuplot: Spline[PieceFunction] => (Option[String], Option[String]) =
    (get: Spline[PieceFunction]) => {
    val defNote = {
      val alphabet = 'a' to 'z'
      alphabet.map(ch => ch + "(x)")
    }

    val changeable =
      (get.sources zip defNote).map{t => {
        val ((Intersection(InclusiveLower(lower), ExclusiveUpper(upper)), func), note) = t
        val definition = s"${note}=${func.toString.replace("^", "**").replace(",", ".")}"
        val interval = f"[${lower}%.10f:${upper}%.10f]".replace(",", ".")
        val plot = interval + " " + note + " ls 1"
        (definition, plot)
      }
      }

    if(changeable.isEmpty) (None, None)
    else{
      val definitions = changeable.map(_._1).reduce(_ + System.lineSeparator() + _)
      val plots = "plot sample " + changeable.map(_._2).reduce(_ + ", " + _)
      (Some(definitions), Some(plots))
    }}

  implicit class Changeable[T](val get: T) extends AnyVal{
    def apply(implicit fn: T => (Option[String], Option[String])): (Option[String], Option[String]) = {
      fn(get)
    }
  }


  abstract class LabelOptions(val whereIs: Char) {

    val label: Option[String]
    val scale: Option[Double]
    val format: Option[String]

    private def lab: Option[String] =  label.map(l => s"set ${whereIs}label "+"\""+s"${l}"+"\"")

    private def sc: Option[String] = scale.map(s => s"set ${whereIs}tics scale ${scale.map(a => f"$a%.2f".replace(",","."))}")

    private def frmt: Option[String] =  format.map(f => s"set ${whereIs}tics format " + "\"" + f + "\"")

    def all: Option[String] = {
      val res = lab ++ sc ++ frmt
      if(res.isEmpty) None
      else Some(res.reduce(_ + System.lineSeparator() + _))
    }
  }

  case class XLabel(label: Option[String], scale: Option[Double], format: Option[String]) extends LabelOptions('x')

  case class YLabel(label: Option[String], scale: Option[Double], format: Option[String]) extends LabelOptions('y')

  case class GPBuilder[T](
                   val source: T,
                   val output: Path,
                   val size: (Double, Double),
                   val xLabel: XLabel = XLabel(None, None, None),
                   val yLabel: YLabel = YLabel(None, None, None),
                   val decSep: Option[Char] = None
                 )(implicit f: T => (Option[String], Option[String])){
    def xlabel(lab: String): GPBuilder[T] = this.copy(xLabel = this.xLabel.copy(label = Some(lab)))
    def ylabel(lab: String): GPBuilder[T] = this.copy(yLabel = this.yLabel.copy(label= Some(lab)))
    def xylabels(xLab: String, yLab: String) = this.copy(xLabel = this.xLabel.copy(label = Some(xLab)),
                                                         yLabel = this.yLabel.copy(label = Some(yLab)))
    def decimalsign(ds: Char): GPBuilder[T] = this.copy(decSep = Some(ds))
    private def decimalSign: Option[String] = decSep.map{ds => s"set decimalsign \'${ds}\'"}

    def run = {

      val dir = Some(s"cd \'${output.getParent.toString}\'")
      val out = Some(s"set output "+"\""+s"${output.getFileName}"+"\"")
      val key = Some("unset key")
      val lineStyle = Some("set linestyle 1 lw 0.5 lc -1 pt 1 ps 4")

      val (definitions, plots) = new Changeable(source).apply

      val terminal = Some(s"set terminal pdfcairo size  ${size._1}cm, ${size._2}cm font" +
        "\"" + "Times-New-Roman,12" + "\"" + " linewidth 1.0 fontscale 0.5")

      val exit = Some("unset output")


      val plottable = {dir :: decimalSign :: xLabel.all :: yLabel.all :: key ::
        lineStyle :: definitions :: terminal :: out :: plots :: exit :: Nil}
      .collect{
        case Some(value) => value
      }


      if(Files.notExists(output.getParent)) Files.createDirectories(output.getParent)
      val fName = output.getFileName.toString
      val gpFileName = fName.take(fName.indexOf('.', fName.size - 5)) + ".gp"
      val gp = output.getParent.resolve(gpFileName)
      if(Files.notExists(gp)) Files.createFile(gp)
      import java.nio.charset._
      val writer = Files.newBufferedWriter(
        gp, Charset.forName("UTF-8"),
        StandardOpenOption.TRUNCATE_EXISTING
      )

      try{
        writer.write(plottable.reduce(_ + System.lineSeparator() + _))
      }
      finally writer.close()

      import scala.sys.process._
      Process({"gnuplot " + "\"" + gpFileName + "\""} :: Nil, output.getParent.toFile).run()
    }
    }

  def apply[T](data: T, path: Path, size: (Double, Double), encoding: String = "UTF-8")(
    implicit f: T => (Option[String], Option[String])): GPBuilder[T] = {
    new GPBuilder(data, path, size, encoding)
  }

  object Spline{
    import java.nio.file._
    def apply(spl: Spline[PieceFunction],
              xLabel: String, yLabel: String,
              path: Path, size: (Double, Double),
              encoding: String = "UTF-8"
             ) = {

      val defNote = {
        val alphabet = 'a' to 'z'
        alphabet.map(ch => ch + "(x)")
      }

      val changeable =
      (spl.sources zip defNote).map{t => {
        val ((Intersection(InclusiveLower(lower), ExclusiveUpper(upper)), func), note) = t
        val definition = s"${note}=${func.toString.replace("^", "**").replace(",", ".")}"
        val interval = f"[${lower}%.10f:${upper}%.10f]".replace(",", ".")
        val plot = interval + " " + note + " ls 1"
        (definition, plot)
        }
      }

      val dir = s"cd \'${path.getParent.toString}\'"

      val decimalSign = "set decimalsign \',\'"

      val xLab = s"set xlabel "+"\""+s"${xLabel}"+"\"" +
        System.lineSeparator() +
        "set xtics scale 0.5" +
        System.lineSeparator() +
        "set xtics format "+"\""+"%g"+"\""

      val yLab = s"set ylabel "+"\""+s"${yLabel}"+"\"" +
        System.lineSeparator() +
        "set ytics scale 0.5" +
        System.lineSeparator() +
        "set xtics format " + "\"" + "%g" + "\""

      val key = "unset key"

      val lineStyle = "set linestyle 1 lw 0.5 lc -1 pt 1 ps 4"

      val definitions = changeable.map(_._1).reduce(_ + System.lineSeparator() + _)

      val terminal = s"set terminal pdfcairo size  ${size._1}cm, ${size._2}cm font" +
        "\"" + "Times-New-Roman,12" + "\"" + " linewidth 1.0 fontscale 0.5"

      val output = s"set output "+"\""+s"${path.getFileName}"+"\""

      val plots = "plot sample " + changeable.map(_._2).reduce(_ + ", " + _)

      val exit = "unset output"

      val plottable = dir :: decimalSign :: xLab :: yLab :: key ::
        lineStyle :: definitions :: terminal :: output :: plots :: exit :: Nil

      if(Files.notExists(path.getParent)) Files.createDirectories(path.getParent)
      val fName = path.getFileName.toString
      val gpFileName = fName.take(fName.indexOf('.', fName.size - 5)) + ".gp"
      val gp = path.getParent.resolve(gpFileName)
      if(Files.notExists(gp)) Files.createFile(gp)
      import java.nio.charset._
      val writer = Files.newBufferedWriter(gp, Charset.forName(encoding),
                                  StandardOpenOption.TRUNCATE_EXISTING)

      try{
        writer.write(plottable.reduce(_ + System.lineSeparator() + _))
      }
      finally writer.close()

      import scala.sys.process._
      Process({"gnuplot " + "\"" + gpFileName + "\""} :: Nil, path.getParent.toFile).run()
    }


  }



}
