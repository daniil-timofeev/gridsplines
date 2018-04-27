
package piecewise

import com.twitter.algebird.Interval.{InLowExUp, InLowInUp}
import com.twitter.algebird.{ExclusiveUpper, InclusiveUpper, Intersection}

package object intervaltree {


  implicit def toClosed[K: Ordering](int: InLowExUp[K]): InLowInUp[K] = {
    Intersection(int.lower, InclusiveUpper(int.upper.upper))
  }

  implicit def toInLowExUp[K: Ordering](int: InLowInUp[K]): InLowExUp[K] = {
    Intersection(int.lower, ExclusiveUpper(int.upper.upper))
  }


}
