import approximation.TwoDCompGrid._

/**
  * Created by Даниил on 13.03.2017.
  */
package object approximation {

  implicit val ortho = new Ortho
  implicit val radial = new Radial
  implicit val X = new XDir
  implicit val Y = new YDir
  implicit val left = new Left
  implicit val right = new Right
}
