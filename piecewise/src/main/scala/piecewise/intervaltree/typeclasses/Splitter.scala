package piecewise.intervaltree
package typeclasses

trait Splitter[K] {

  def split[V](v: ((K, K), V),
               at: Interval[K, Closed, Closed],
               maxSize: K): Iterator[((K, K), V)]

}
object Splitter {

}
