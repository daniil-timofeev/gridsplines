package minimization

import scala.annotation.tailrec
import scala.math._

/** Поиск минимума при помощи метода золотого сечения.
  * Find minimum with the golden slice method application
  * Created by Даниил on 29.07.2016.
  */
object GoldenSlice {

  /** Поиск минимума при помощи метода зологого сечения.
    * Find minimum with the golden slice method application
    *
    * @param x01 левая граница минимизации. Left searching bound
    * @param x02 правая граница минимизации. Right searching bound
    * @param f функция минимизации. Minimized function
    * @return значение x минимума. x of the minimum
    */
  def apply(x01: Double, x02: Double, f: (Double) => Double): Double = {
    assert(!x01.isNaN || !x02.isNaN, s"left boundary ${x01} or right boundary ${x02} must be not NaN")
    assert(x01 < x02, f"left boundary ${x01}%.2f must be lower, than right boundary ${x02}%.2f")
    val fx01 = f(x01)
    val fx02 = f(x02)
    val x11 = x01 + 0.38*(x02 -x01)
    val x22 = x02 - 0.38*(x02 - x01)

    @inline
    def solver(fx1 : Double, fx2 : Double, fx3 : Double, fx4 : Double) : Int = {
      var min : Int = 0
      @inline
      val first = if(fx1 < fx2){
        min = 0
        fx1}
      else {min = 1
        fx2}
      @inline
      val second = if(first < fx3) first
      else{
        min = 2
        fx3}
      @inline
      val third = if(second < fx4) second
      else {min = 3
      fx4}
      min
    }

    @tailrec
    def goldenSliceNext(x1 : Double, x2 : Double, x3 : Double,
                        fx1 : Double, fx2 : Double, fx3 : Double, f : (Double) => Double) : Double = {
      @inline
      def ready = {abs(x1 - x2) < 0.0001 || abs(x2 - x3) < 0.0001}
      val x = x3 + x1 - x2
      if(x < x2) {
        val fx = f(x)
        val res = solver(fx1, f(x), fx2, fx3)
        if(ready) List(x1, x, x2, x3)(res)
        else if(res == 1) goldenSliceNext(x1, x, x2, fx1, fx, fx2, f)
        else goldenSliceNext(x, x2, x3, fx, fx2, fx3, f)
      }
      else {
        val fx = f(x)
        val res = solver(fx1, fx2, fx , fx3)
        if (ready) List(x1, x2, x, x3)(res)
        else if(res == 1) goldenSliceNext(x1, x2, x, fx1, fx2, fx, f)
        else goldenSliceNext(x2, x, x3, fx2, fx, fx3, f)
      }
    }
    val fx11 = f(x11)
    val fx22 = f(x02)

    solver(fx01, fx11, fx22, fx02) match{
      case 0 =>  x01
      case 1 =>  goldenSliceNext(x01, x11, x22, fx01, fx11, fx22, f)
      case 2 =>  goldenSliceNext(x11, x22, x02, fx11, fx22, fx02, f)
      case 3 =>  x02
    }
  }

}
