
import org.scalacheck._
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._
import scala.math._
import piecewise.PieceFunction._
/**
  * Created by Даниил on 17.02.2017.
  */
object RuleOfGorner extends Properties("Rule of Gorner implementation") {

  property("Square Gorner coincidence") = Prop.forAll{ (a0: Double, a1: Double, a2: Double, x: Double) => {
    val genGorner = ruleOfGorner(x, a2, a1, a0)
    val specGorner = quadraticRuleOfGorner(x, a0, a1, a2)
    val accurate = polynomial(x, a2, a1, a0)
    all(
      //abs((genGorner - accurate) / accurate) < 1.00,
      abs((specGorner - accurate) / accurate) < 1.00
    )
  }}

  property("Cubic Gorner coincidence") = Prop.forAll{ (a0: Double, a1: Double, a2: Double, a3: Double, x: Double) => {
    val genGorner = ruleOfGorner(x, a3, a2, a1, a0)
    val specGorner = cubicRuleOfGorner(x, a0, a1, a2, a3)
    val accurate = polynomial(x, a3, a2, a1, a0)
    all(
      //abs((genGorner - accurate) / accurate) < 1.00,
      abs((specGorner - accurate) / accurate) < 1.00
    )
  }}

  property("Quad Gorner coincidence") =
    Prop.forAll{(a0: Double, a1: Double, a2: Double, a3: Double, a4: Double, x: Double) => {
      val genGorner = ruleOfGorner(x, a4, a3, a2, a1, a0)
      val specGorner = quadRuleOfGorner(x, a0, a1, a2, a3, a4)
      val accurate = polynomial(x, a4, a3, a2, a1, a0)
      all(
        //abs((genGorner - accurate) / accurate) < 1.00,
        abs((specGorner - accurate) / accurate) < 1.00
      )
    }}
}

