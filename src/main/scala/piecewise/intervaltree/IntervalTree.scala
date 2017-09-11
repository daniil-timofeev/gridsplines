package piecewise.intervaltree

import com.twitter.algebird._
import com.twitter.algebird.Interval.InLowExUp
import com.twitter.algebird.Interval.MaybeEmpty.{NotSoEmpty, SoEmpty}

import scala.Option
import com.twitter.algebird.monad._
import com.twitter.algebird.monad.Trampoline._
import piecewise._

import scala.annotation.tailrec
import scala.collection.Iterator



/**
  * Created by Даниил on 24.03.2017.
  */
abstract class IntervalTree[K: Ordering, +V](val interval: InLowExUp[K], val v: V){

  def apply = v

  def contains(x: K) = interval.contains(x)

  def upperThan(x: K): Boolean = !interval.lower.contains(x)

  def lowerThan(x: K): Boolean = !interval.upper.contains(x)

  def tuple: (InLowExUp[K], V) = (interval, v)

  def sliceUpper(x: K): IntervalTree[K, V]

  def sliceLower(x: K): IntervalTree[K, V]
  import scala.collection._

  def collect[T](pf: PartialFunction[(InLowExUp[K], V), T])(
    implicit bi: mutable.Builder[T, Iterator[T]]): mutable.Builder[T, Iterator[T]]

  def mapNonDecreasing[U: Ordering](i: K => U): IntervalTree[U, V]

  def size: Int

  def sumBy[T: Monoid](low: K, upp: K, f: (K, K, V) => T): T

  def map[V1](v: V => V1): IntervalTree[K, V1]

  def map[U: Ordering, V1](
    f : (InLowExUp[K], V) => (InLowExUp[U], V1)): IntervalTree[U, V1]

  def map[V1](f: (K, K, V) => V1): IntervalTree[K, V1]

  def splitWhere(f: (K, K, V) => Int)(
    implicit field: Field[K]): Option[IntervalTree[K, V]] = {
    import com.twitter.algebird.Operators._
    val src = iterator.flatMap{tuple =>
      val (interval, pieceFunc) = tuple
      val low = interval.lower.lower
      val upp = interval.upper.upper
      val parts = f(low, upp, pieceFunc)
      if(parts > 1) {
        val length = upp - low
        val newLength = field.div(length, field.fromInt(parts))
        Iterator.range(0, parts).map{i =>
          val newLow = low + newLength * field.fromInt(i)
          val newUpp = upp + newLength
          Interval.leftClosedRightOpen(newLow, newUpp) match{
            case notEmpty: NotSoEmpty[K, InLowExUp] => (notEmpty.get, pieceFunc)
            case _ : SoEmpty[K, InLowExUp] => ???
          }
        }
      } else Iterator.single((interval, pieceFunc))
    }.toIterable
    val size = src.size
   IntervalTree.buildLeft(src.iterator, size)
  }

  def mapIterator[U: Ordering, V1](
    f: (InLowExUp[K], V) => (Iterator[(InLowExUp[U], V1)], Int)
  ): (Iterator[(InLowExUp[U], V1)], Int)

  def buildIterator[V1 >: V](array: Array[(InLowExUp[K], V1)], start: Int)
  : Array[(InLowExUp[K], V1)]

  def iterator: Iterator[(InLowExUp[K], V)]

  def intervalLength(implicit g: Group[K]): K

}

final case class InternalNode[K: Ordering, V]( override val interval: InLowExUp[K],
                                               override val v: V,
                                               left: Option[IntervalTree[K, V]],
                                               right: Option[IntervalTree[K, V]])
  extends IntervalTree[K, V](interval, v){
  def this(map: (InLowExUp[K], V),
           left: Option[IntervalTree[K, V]],
           right: Option[IntervalTree[K, V]]){
    this(map._1, map._2, left, right)
  }

  override lazy val size: Int =
    1 + left.map(_.size).getOrElse(0) + right.map(_.size).getOrElse(0)

  def sliceUpper(x: K): IntervalTree[K, V] = {
    x match{
      case center if interval.contains(x) => {
        this.copy(interval =
          Intersection.apply(interval.lower, interval.upper.copy(center)),
          right = None)
      }
      case l if left.nonEmpty && interval.upper.contains(l) => {
        left.get.sliceUpper(l)
      }
      case r if right.nonEmpty && interval.lower.contains(r) => {
        this.copy(right = right.map(old => old.sliceUpper(r)))
      }
    }
  }

  def sliceLower(x: K): IntervalTree[K, V] = {
    x match{
      case center if interval.contains(x) => {
        this.copy(interval = Intersection.apply(interval.lower.copy(center),
          interval.upper), left = None)
      }
      case l if left.nonEmpty && interval.upper.contains(l) => {
        this.copy(left = left.map(old => old.sliceLower(l)))
      }
      case r if right.nonEmpty && interval.lower.contains(r) => {
        right.get.sliceLower(r)
      }
    }
  }

  import scala.collection._
  override
  def collect[T](pf: PartialFunction[(InLowExUp[K], V), T])(
    implicit bi: mutable.Builder[T, Iterator[T]]): mutable.Builder[T, Iterator[T]] = {
    left.foreach(_.collect(pf))
    if(pf.isDefinedAt((interval, v))) bi += pf((interval, v))
    right.foreach(_.collect(pf))
    bi
  }

  override final def mapNonDecreasing[U: Ordering](i: (K) => U): InternalNode[U, V] = {
      interval.mapNonDecreasing(i) match {
        case e: Empty[U] => ???
        case u: Universe[U] => ???
        case newInterval: InLowExUp[U] => {
          new InternalNode[U, V](
            newInterval,
            v,
            left.map(iTree => iTree.mapNonDecreasing[U](i)),
            right.map(iTree => iTree.mapNonDecreasing[U](i))
          )
        }
      }
  }

  override final def map[V1](f: (V) => V1): InternalNode[K, V1] = {
    val contains = f(v)
    new InternalNode[K, V1](
      interval,
      contains,
      left.map(iTree => iTree.map(f)),
      right.map(iTree => iTree.map(f))
    )
  }

  override final def map[U: Ordering, V1](
                   f: (InLowExUp[K], V) => (InLowExUp[U], V1)): InternalNode[U, V1] = {
    val tuple = f(interval, v)
    new InternalNode[U, V1](tuple,
      left.map(_.map(f)),
      right.map(_.map(f))
    )
  }

  override final def map[V1](f: (K, K, V) => V1): InternalNode[K, V1] = {
    val low = interval.lower.lower
    val upp = interval.upper.upper
    new InternalNode[K, V1](
      interval,
      f(low, upp, v),
      left.map(_.map(f)),
      right.map(_.map(f))
    )
  }

  override
  def mapIterator[U: Ordering, V1](
    f: (InLowExUp[K], V) => (Iterator[(InLowExUp[U], V1)], Int)
    ): (Iterator[(InLowExUp[U], V1)], Int) = {
    val res = f(interval, v)
    if (left.isEmpty) {
      if (right.isEmpty) res
      else {
        val (result, s) = res
        val r = right.get.mapIterator(f)
        (result ++ r._1, s + r._2)
      }
    }
    else if (right.isEmpty) {
      val (result, s) = res
      val l = left.get.mapIterator(f)
      (l._1 ++ result, s + l._2)
    }
    else {
      val (result, s) = res
      val l = left.get.mapIterator(f)
      val r = right.get.mapIterator(f)
      (l._1 ++ result ++ r._1, l._2 + s + r._2)
    }
  }

  def buildIterator[V1 >: V](array: Array[(InLowExUp[K], V1)], start: Int)
  : Array[(InLowExUp[K], V1)] = {
    var idx = start
    if (left.nonEmpty) {
      left.get.buildIterator(array, start)
      idx += left.get.size
    }
    array.update(idx, tuple)
    idx += 1
    if (right.nonEmpty) {
      right.get.buildIterator(array, idx)
    }
    array
  }

  def iterator: Iterator[(InLowExUp[K], V)] = {
    val array = new Array[(InLowExUp[K], V)](size)
    buildIterator(array, 0).iterator
  }

  override lazy val toString: String = {
    val l = left.map(_.toString).getOrElse(" ")
    val r = right.map(_.toString).getOrElse(" ")
    l +
    "[" +
    interval.lower.lower.toString +
    interval.upper.upper.toString +
    ") :" + v.toString + System.lineSeparator() +
    r
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case tree: InternalNode[Any, Any] => {
        val l =
          if (this.left.isEmpty && tree.left.nonEmpty ||
            this.left.nonEmpty && tree.left.isEmpty) false
          else {
            if (this.left.isEmpty && tree.left.isEmpty) true
            else this.left.get.equals(tree.left.get)
          }

        val r =
          if (this.right.isEmpty && tree.right.nonEmpty ||
            this.right.nonEmpty && tree.right.isEmpty) false
          else {
            if (this.right.isEmpty && tree.right.isEmpty) true
            else this.right.get.equals(tree.left.get)
          }

        l &&
        this.interval.equals(tree.interval) && this.v.equals(tree.v) &&
        r
      }
      case _ => false
    }
  }

  def intervalLength(implicit g: Group[K]): K = {
    g.remove(upper, lower)
  }

  def upper: K = {
    right match {
      case None => {
        interval.upper.upper
      }
      case Some(i @ InternalNode(_, _, _, _)) => {
        i.upper
      }
      case Some(Leaf(Intersection(_, ExclusiveUpper(upp: K)), _)) => {
        upp
      }
    }
  }

  def lower: K = {
    left match {
      case None => {
        interval.lower.lower
      }
      case Some(i @ InternalNode(_, _, _, _)) => {
        i.lower
      }
      case Some(Leaf(Intersection(InclusiveLower(low: K), _), _)) => {
        low
      }
    }
  }


  def sumBy[T: Monoid](low: K, upp: K, f: (K, K, V) => T): T = {
    val m = implicitly[Monoid[T]]
    if (interval.contains(low) && interval.contains(upp)) f(low, upp, v)
    else if (interval.contains(low)) {
      val thisVal = f(low, interval.upper.upper, v)
      if (right.nonEmpty) m.plus(thisVal, right.get.sumBy(low, upp, f))
      else thisVal
    }
    else if (interval.contains(upp)) {
      val thisVal = f(interval.lower.lower, upp, v)
      if (left.nonEmpty) m.plus(thisVal, left.get.sumBy(low, upp, f))
      else thisVal
    }
    else {
      val thisVal = f(interval.lower.lower, interval.upper.upper, v)
      val l =
        if (left.nonEmpty) left.get.sumBy(low, upp, f)
        else m.zero
      val r =
        if (right.nonEmpty) right.get.sumBy(low, upp, f)
        else m.zero
      m.plus(m.plus(l, thisVal), r)
    }
  }

}

final case class Leaf[K: Ordering, V](override val interval: InLowExUp[K],
                                      override val v: V)
  extends IntervalTree[K, V](interval, v){

  def this(map: (InLowExUp[K], V)){
    this(map._1, map._2)
  }

  def sliceUpper(x: K): IntervalTree[K, V] = {
    if(interval.contains(x)) this.copy(interval =
      Intersection.apply(interval.lower, interval.upper.copy(x)))
    else this //assumed what intervals  at tree are continuous
  }

  def sliceLower(x: K): IntervalTree[K, V] = {
    if(interval.contains(x)) this.copy(interval =
      Intersection.apply(interval.lower.copy(x), interval.upper))
    else this //assumed what intervals at tree are continuous
  }

  override lazy val size: Int = 1

  import scala.collection._
  def collect[T](pf: PartialFunction[(InLowExUp[K], V), T])(
    implicit bi: mutable.Builder[T, Iterator[T]]): mutable.Builder[T, Iterator[T]] = {
    if(pf.isDefinedAt((interval, v))) bi += pf((interval, v))
    bi
  }

  override def mapNonDecreasing[U: Ordering](i: (K) => U): Leaf[U, V] = {
    interval.mapNonDecreasing(i) match {
      case u: Universe[U] => ???
      case empty: Empty[U] => ???
      case i: InLowExUp[U] => {
        new Leaf[U, V](i, v)
      }
    }
  }

  override def map[V1](f: (V) => V1): Leaf[K, V1] = {
    this.copy(v = f(v))
  }

  override def map[U: Ordering, V1](f: (InLowExUp[K], V) =>
    (InLowExUp[U], V1)): Leaf[U, V1] = {
    new Leaf[U, V1](f(interval, v))
  }

  override def map[V1](f: (K, K, V) => V1): Leaf[K, V1] = {
    val low = interval.lower.lower
    val upp = interval.upper.upper
    new Leaf[K, V1](interval, f(low, upp, v))
  }

  override
  def mapIterator[U: Ordering, V1](
              f: (InLowExUp[K], V) => (Iterator[(InLowExUp[U], V1)], Int)
              ): (Iterator[(InLowExUp[U], V1)], Int) = {
    f(interval, v)
  }


  def buildIterator[V1 >: V](array: Array[(InLowExUp[K], V1)], start: Int)
  : Array[(InLowExUp[K], V1)] = {
    array.update(start, tuple)
    array
  }

  def iterator: Iterator[(InLowExUp[K], V)] = Iterator.single(tuple)

  override def equals(obj: scala.Any): Boolean = {
    obj match{
    case leaf: Leaf[Any, Any] => {
      this.interval.equals(leaf.interval) && this.v.equals(leaf.v)
    }
    case _ => false
  }}

  override def toString =
    s"[${interval.lower.lower}, ${interval.upper.upper}):" +
      s" ${v.toString}" + System.lineSeparator()

  def intervalLength(implicit g: Group[K]): K = {
    g.remove(interval.upper.upper, interval.lower.lower)
  }

  override def sumBy[T: Monoid](low: K, upp: K, f: (K, K, V) => T): T = {
    if (interval.contains(low) && interval.contains(upp)) f(low, upp, v)
    else if (interval.contains(low)) f(low, interval.upper.upper, v)
    else if (interval.contains(upp)) f(interval.lower.lower, upp, v)
    else if (interval.lower.contains(upp) && interval.upper.contains(low))
      f(interval.lower.lower, interval.upper.upper, v)
    else implicitly[Monoid[T]].zero
  }

}

object IntervalTree{

  def buildOne[V](xLow: Double, xUpp: Double, f: V): Option[Leaf[Double, V]] = {
      Interval.leftClosedRightOpen(xLow, xUpp) match {
        case notEmpty: NotSoEmpty[Double, InLowExUp] => Some(new Leaf(notEmpty.get, f))
        case _ => None
      }
  }

  @tailrec
  def find[K: Ordering, V](x: K, tree: Option[IntervalTree[K, V]])
  : Option[IntervalTree[K, V]] = {
    if(tree.nonEmpty) tree.get match{
      case internal: InternalNode[K, V] =>{
        x match{
          case center if internal.interval.contains(center) => tree
          case left if internal.left.nonEmpty &&
                       internal.upperThan(left) =>
            find(x, internal.left)
          case right if internal.right.nonEmpty &&
                        internal.lowerThan(right) =>
            find(x, internal.right)
          case _  => None
        }
      }
      case leaf: Leaf[K, V] => {
        if(leaf.interval.contains(x)) tree else None
    }
  } else None
}

  def apply[K: Ordering, V](vect: List[(InLowExUp[K], V)])
  : Option[IntervalTree[K, V]] = {

    implicit object Ordering extends Ordering[InLowExUp[K]]{
      override def compare(x: InLowExUp[K], y: InLowExUp[K]): Int =
        if(x.lower.contains(y.lower.lower)){
          if(x.upper.contains(y.upper.upper)) 0
          else - 1
        }
        else if(y.lower.contains(x.lower.lower) && y.upper.contains(x.upper.upper)) 0
        else 1
    }

    val sorted = vect.sortBy(t => t._1)
    Trampoline.run(buildLeft(sorted))
  }


  @tailrec
  final def findNode[K, V](tree: Option[IntervalTree[K, V]], low: K, upp: K)(
    implicit ord: Ordering[K]): Option[IntervalTree[K, V]] = {
    tree match {
      case Some(InternalNode(
      Intersection(InclusiveLower(lower), ExclusiveUpper(upper)),
      _, left, right)) => {
        if (ord.gt(low, upper)) findNode(right, low, upp)
        else if(ord.lt(upp, lower)) findNode(left, low, upp)
        else tree
      }
      case Some(Leaf(Intersection(InclusiveLower(lower), ExclusiveUpper(upper)), _)) => {
        if (ord.lt(low, upper) && ord.gt(upp, lower)) tree
        else None
      }
      case None => None
    }
  }

  def area[K: Ordering, V](lower: K, upper: K, tree: Option[IntervalTree[K, V]])
  : Trampoline[List[(InLowExUp[K], V)]] = {
    def contain(interval: InLowExUp[K]): Boolean = {
      interval.lower.contains(upper) || interval.upper.contains(lower)
    }
    //TODO improve speed
    tree match{
      case Some(InternalNode(interval, v, left, right)) => {
        val mid = if(contain(interval)) (interval, v) :: Nil else Nil
        for{
          l <- call(area[K, V](lower, upper, left))
          r <- call(area[K, V](lower, upper, right))
        }yield{ l ::: mid ::: r }
      }
      case Some(Leaf(interval, v)) if contain(interval) => Done((interval, v) :: Nil)
      case _ => Done(Nil)
    }
  }

  def toList[K: Ordering, V](tree: Option[IntervalTree[K, V]],
                             buffer: collection.mutable.ListBuffer[(InLowExUp[K], V)])
  : Trampoline[collection.mutable.ListBuffer[(InLowExUp[K], V)]] = {
    tree match{
      case None => Done(buffer)
      case Some(InternalNode(interval, v, left, right)) => {
        for {
          l <- call(toList(left, buffer))
          c <- Done(l.+=((interval, v)))
          r <- call(toList(right, c))
        } yield r
      }
      case Some(Leaf(interval, v)) => Done(buffer.+=((interval, v)))
    }
}

  final def buildLeft[K: Ordering, V](vals: List[(InLowExUp[K], V)])
  : Trampoline[Option[IntervalTree[K, V]]] = {
    vals match{
      case one :: Nil => Done(Some(new Leaf[K, V](one)))
      case leaf :: internal :: Nil => {
        val l = new Leaf(leaf)
        Done(Some(new InternalNode[K, V](internal, Some(l), None)))
      }
      case leftLeaf :: internal :: rightLeaf :: Nil => {
        val left = new Leaf(leftLeaf)
        val right = new Leaf(rightLeaf)
        Done(Some(new InternalNode(internal, Some(left), Some(right))))
      }
      case default: List[((InLowExUp[K], V))] => {
        val size = vals.size
        val leftIndex =
          if(size % 2 == 0) (size + 1) / 2
          else size / 2
        val rightSize = size - leftIndex - 1
        for{
          leftNode <- call(buildLeft(default.take(leftIndex)))
          rightNode <- call(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftIndex), leftNode, rightNode))
      }
    }
  }

  final def buildLeft[K: Ordering, V](vals: Iterator[(InLowExUp[K], V)],
                                      size: Int): Option[IntervalTree[K, V]] = {
    if(vals.isEmpty){
      None
    } else{
      size match {
        case 1 => Some(new Leaf(vals.next()))
        case 2 => {
          val left = vals.next()
          val mid = vals.next()
          Some(new InternalNode(mid, Some(new Leaf(left)), None))
        }
        case 3 => {
          val left = vals.next()
          val mid = vals.next()
          val right = vals.next()
          Some(new InternalNode(mid, Some(new Leaf(left)), Some(new Leaf(right))))
        }
        case s => {
          val leftIndex =
            if(s % 2 == 0) (size + 1) / 2
            else s / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val mid = vals.next()
          val right = buildRight(vals, rightSize)
          Some(new InternalNode[K, V](mid, left, right))
        }
      }
  }
  }

  final def buildRight[K: Ordering, V](vals: List[(InLowExUp[K], V)])
  : Trampoline[Option[IntervalTree[K, V]]] = {
    vals match{
      case one :: Nil => Done(Some(new Leaf(one)))
      case internal :: leaf :: Nil => {
        val l = new Leaf(leaf)
        Done(Some(new InternalNode[K, V](internal, None, Some(l))))
      }
      case leftLeaf :: internal :: rightLeaf :: Nil=> {
        val left = new Leaf(leftLeaf)
        val right = new Leaf(rightLeaf)
        Done(Some(new InternalNode[K, V](internal, Some(left), Some(right))))
      }
      case default: List[((InLowExUp[K], V))] => {
        val size = vals.size
        val leftIndex =
          if(size % 2 == 0) (size - 1) / 2
          else size / 2
        val rightSize = size - leftIndex - 1
        for{
          leftNode <- call(buildLeft(default.take(leftIndex)))
          rightNode <- call(buildRight(default.takeRight(rightSize)))
        } yield Some(new InternalNode[K, V](default(leftIndex), leftNode, rightNode))
      }
    }
  }

  final def buildRight[K: Ordering, V](vals: Iterator[(InLowExUp[K], V)],
                                       size: Int): Option[IntervalTree[K, V]] = {
    if(vals.isEmpty){
      None
    } else{
      size match {
        case 1 => Some(new Leaf(vals.next()))
        case 2 => {
          val mid = vals.next()
          val right = vals.next()
          Some(new InternalNode(mid, None, Some(new Leaf(right))))
        }
        case 3 => {
          val left = vals.next()
          val mid = vals.next()
          val right = vals.next()
          Some(new InternalNode(mid, Some(new Leaf(left)), Some(new Leaf(right))))
        }
        case s => {
          val leftIndex =
            if(size % 2 == 0) (size - 1) / 2
            else size / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val mid = vals.next()
          val right = buildLeft(vals, rightSize)
          Some(new InternalNode[K, V](mid, left, right))
        }
      }
    }
  }
}