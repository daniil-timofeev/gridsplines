
import org.scalacheck.Prop._
import org.scalacheck._
import piecewise.PieceFunction._
import org.scalacheck.Gen._
import scala.math._
/**
  * rule of Horner tests
  */
object RuleOfHorner extends Properties("Rule of Horner implementation") {

  implicit val double = choose(0.0, 10.0)

  property("Square Horner coincidence") =
    Prop.forAllNoShrink(double, double, double, double){
      (a0: Double, a1: Double, a2: Double, x: Double) => {
    val genGorner = ruleOfHorner(x, a0, a1, a2)
    val specGorner = quadraticRuleOfHorner(x, a0, a1, a2)
    val accurate = polynomial(x, a2, a1, a0)
    all(
      abs((genGorner - accurate) / accurate) < 3.00,
      abs((specGorner - accurate) / accurate) < 3.00
    )
  }}

  property("Cubic Horner coincidence") =
    Prop.forAllNoShrink(double, double, double, double, double){
      (a0: Double, a1: Double, a2: Double, a3: Double, x: Double) => {
    val genGorner = ruleOfHorner(x, a0, a1, a2, a3)
    val specGorner = cubicRuleOfHorner(x, a0, a1, a2, a3)
    val accurate = polynomial(x, a3, a2, a1, a0)
    all(
      abs((genGorner - accurate) / accurate) < 3.00,
      abs((specGorner - accurate) / accurate) < 3.00
    )
  }}

  property("Quad Horner coincidence") =
    Prop.forAllNoShrink(double, double, double, double, double, double){
      (a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, x: Double) => {
      val genGorner = ruleOfHorner(x, a0, a1, a2, a3, a4)
      val specGorner = quadRuleOfHorner(x, a0, a1, a2, a3, a4)
      val accurate = polynomial(x, a4, a3, a2, a1, a0)
      all(
        abs((genGorner - accurate) / accurate) < 3.00,
        abs((specGorner - accurate) / accurate) < 3.00
      )
    }}
}

