
import org.scalacheck._
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop._
import scala.math._
import PieceFunction._
/**
  * Created by Даниил on 17.02.2017.
  */
object RuleOfGorner extends Properties("Rule of Gorner implementation"){

  property("Square Gorner coincidence") = Prop.forAll{(a0: Double, a1: Double, a2: Double, x: Double) =>{
    val genGorner = ruleOfGorner(x, a0, a1, a2)
    val specGorner = squaredRuleOfGorner(x, a0, a1, a2)
    val accurate = polynomial(x, a0, a1, a2)
    all(
      abs((genGorner - accurate)/accurate) < 0.01,
      abs((specGorner - accurate)/accurate) < 0.01
    )
  }}

  property("Cubic Gorner coincidence") = Prop.forAll{(a0: Double, a1: Double, a2: Double, a3: Double, x: Double) =>{
    val genGorner = ruleOfGorner(x, a0, a1, a2, a3)
    val specGorner = cubicRuleOfGorner(x, a0, a1, a2, a3)
    val accurate = polynomial(x, a0, a1, a2, a3)
    all(
      abs((genGorner - accurate)/accurate) < 0.01,
      abs((specGorner - accurate)/accurate) < 0.01
    )
  }}

}
