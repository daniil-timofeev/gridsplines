package piecewise.intervaltree
package typeclasses

trait Splitter[K] {

  def split[V](v: ((K, K), V),
               at: Interval[K, Closed, Closed],
               maxSize: K): Iterator[((K, K), V)]

}
object Splitter {

  implicit val DoubleSplitter = new Splitter[Double] {

    override def split[V](
      v: ((Double, Double), V),
      at: Interval[Double, Closed, Closed],
      maxSize: Double): Iterator[((Double, Double), V)] = {
       val ((l, u), value) = v
        if (at.contains(l) && at.contains(u)) {
          val abs = u - l
          val n = math.ceil(abs / maxSize).toInt
          val step = abs / n

          Iterator.iterate(l)(l => l + step)
            .take(n)
            .map(l => ((l, l + step), value))
        }
        else {
          Iterator.single(v)
        }
    }
  }


}
