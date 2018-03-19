package approximation.passion

/**
  *
  */
class Passion(size: Int) {

  /**  Solve a system of linear equation formed into tridiagonal matrix with Thomas algorithm
    * @param lists matrix values, length x 3
    * @param vector vector values
    * @param result result array
    */
  def solve(lists : Array[Array[Double]], vector : Array[Double], result: Array[Double]) : Unit = {
    backwardPassion(forwardPassion(lists, vector, result), result)
  }

  private[this] val afterForwardPassion : Array[Array[Double]] = Array.fill(size, 2)(0.0)

  /** Forward passion, which determine lambda and delta coefficients
    * @param array matrix
    * @param vector vector values
    * @return fitst -- delta, second -- lambda */
  protected final def forwardPassion(array : Array[Array[Double]], vector : Array[Double], result : Array[Double]):
  Array[Array[Double]] = {
    var i = 0
    val LAST = result.length - 1
    while(i < result.length){
      val b = array(i)(0)
      val c = array(i)(1)
      val d = array(i)(2)
      val vect = vector(i)
      i match{
        case 0 => {
          forwardFirst(c, d, vect, afterForwardPassion(i))
        }
        case LAST => {
          forwardLast(b, c, d, vect,
            afterForwardPassion(i-1)(0), afterForwardPassion(i-1)(1), afterForwardPassion(i))
        }
        case _ => {
          forwardUnit(b, c, d, vect,
            afterForwardPassion(i-1)(0), afterForwardPassion(i-1)(1), afterForwardPassion(i))
        }
      }
      i += 1
    }
    afterForwardPassion
  }
}
