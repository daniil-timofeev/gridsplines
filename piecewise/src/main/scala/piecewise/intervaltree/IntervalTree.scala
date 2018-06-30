package piecewise.intervaltree



import cats.Show

import scala.annotation.tailrec
import scala.collection.Iterator
import scala.language.higherKinds
/**
  * AST for the interval tree algorithm
  *
  */
abstract class IntervalTree[K, +V]{

  /** size of the interval tree. */
  def size: Int

  def hasBoundNode: Boolean

  def sliceLower(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V]

  def sliceUpper(x: K)(implicit ord: Ordering[K]): IntervalTree[K, V]

  def upperThan(x: K)(implicit ord: Ordering[K]): Boolean

  def lowerThan(x: K)(implicit ord: Ordering[K]): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int): Array[((K, K), V1)]

  def array[V1 >: V]: Array[((K, K), V1)] = {
    this match {
      case empty: EmptyNode[K, V] => Array.empty[((K, K), V1)]
      case branch: AbstractInternalNode[K, V1, _] =>
        updateArray(new Array[((K, K), V1)](size), branch.left.size)
      case leaf: AbstractLeaf[K, V1, _] => Array.apply[((K, K), V1)](leaf.values)
    }
  }

  def iterator: Iterator[((K, K), V)] = {
    array.iterator
  }

  def rebuild(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    this match {
      case empty: EmptyNode[K, V] => empty
      case nonEmpty: NonEmptyITree[K, V, Upper] => {
        IntervalTree.buildRight(nonEmpty.iterator, size)
      }
    }
  }

  def mapOption[R](f: NonEmptyITree[K, V, _] => R): Option[R] = {
    this match {
      case empty: EmptyNode[K, V] => None
      case nonEmpty: NonEmptyITree[K, V, _] => Some(f(nonEmpty))
    }
  }

}

case class EmptyNode[K, +V]() extends IntervalTree[K, V]{

  val size = 0

  override def hasBoundNode = false

  override def upperThan(x: K)(implicit ord: Ordering[K]) = true

  override def lowerThan(x: K)(implicit ord: Ordering[K]) = true

  override def equals(that: Any) = that match{
    case empty: EmptyNode[k, v] => true
    case other => false
  }

  override val isEmpty: Boolean = true

  override private[intervaltree]
  def updateArray[V1 >: V](array: Array[((K, K), V1)], pos: Int
        ): Array[((K, K), V1)] = {
    array
  }

  override def toString: String = ""

  def sliceLower(x: K)(
    implicit ord: Ordering[K]): IntervalTree[K, V] = this

  def sliceUpper(x: K)(
    implicit ord: Ordering[K]): IntervalTree[K, V] = this

}

abstract class NonEmptyITree[K, +V, U <: BoundType]
  extends IntervalTree[K, V]{

  val interval: Interval[K, Closed, U]
  val v: V

  def contains(x: K)(implicit ord: Ordering[K]): Boolean

  def apply: V = v

  def bimap[C, D](f: K => C, g: V => D): IntervalTree[C, D] = {
    IntervalTree.bifunctor.bimap(this)(f, g)
  }

  import typeclasses._
  def split(at: Interval[K, Closed, Closed])(
    maxSize: K,
  )(implicit S: Splitter[K], ord: Ordering[K]): IntervalTree[K, V] = {
    val arr = iterator.flatMap(v => S.split(v, at, maxSize)).toArray
    val size = arr.size
    IntervalTree.buildLeft(arr.iterator, size)
  }

  def tuple: (Interval[K, Closed, U], V) = (interval, v)

  def values: ((K, K), V) = (interval.values, v)

  def low: K = interval.lower.bound
  def upp: K = interval.upper.bound
  def extract: ((K, K), V) = ((low, upp), v)

  import cats._
  def intervalLength(implicit G: Group[K]): K = {
    G.remove(upp, low)
  }

  def upperBound: Bound[K, Closed, Upper]

  def lowerBound: Bound[K, Closed, Lower]

  def wholeInterval: Interval[K, Closed, Closed] = {
    new Interval[K, Closed, Closed](lowerBound, upperBound)
  }

  override val isEmpty: Boolean = false

}
import cats.Bifunctor
object IntervalTree {

  private[intervaltree]
  def convertMostRight[K, V](tree: IntervalTree[K, V]): IntervalTree[K, V] = {
    tree match {
      case empty: EmptyNode[K, V] => empty
      case InternalNode(i, v, left, right) => {
        right match{
          case empty: EmptyNode[K, V] => {
            left match {
              case empty: EmptyNode[K, V] => new UpperBoundLeaf(i.toClosed, v)
              case nonEmpty: NonEmptyITree[K, V, Open] =>
                new UpperBoundInternalNode(i.toClosed, v, nonEmpty)
            }
          }
          case nonEmpty: NonEmptyITree[K, V, Open] => {
              new InternalNode(i, v, left, convertMostRight(right))
            }
          }
      }
      case Leaf(i, v) => new UpperBoundLeaf(i.toClosed, v)
      case upperLeaf: UpperBoundLeaf[K, V] => upperLeaf
    }
  }
  private[intervaltree]
  def buildUpperBound[K: Ordering, V](
    low: K,
    upp: K,
    v: V,
    left: IntervalTree[K, V]): IntervalTree[K, V] = {

    Interval[K, Closed, Closed](low, Closed, upp, Closed) match {
      case empty: Empty[K, Closed, Closed] => {
        left match {
          case empty: EmptyNode[K, V] => empty
          case nonEmptyTree: NonEmptyITree[K, V, Open] => {
            convertMostRight[K, V](nonEmptyTree)
          }
        }
      }
      case nonEmpty: Interval[K, Closed, Closed] => {
        left match {
          case empty: EmptyNode[K, V] => new UpperBoundLeaf[K, V](nonEmpty.toClosed, v)
          case nonEmptyTree: NonEmptyITree[K, V, Open] => {
            new UpperBoundInternalNode[K, V](nonEmpty.toClosed, v, nonEmptyTree)
          }
        }
      }
    }
  }



  private[intervaltree]
  def posOfLeft[K, V](tree: IntervalTree[K, V], parentPos: Int): Int = {
    tree match {
      case empty: EmptyNode[K, V] => parentPos - 1
      case InternalNode(_, _, _, right) => {
        parentPos - right.size - 1
      }
      case UpperBoundInternalNode(_, _, left) => parentPos - 1
      case l: Leaf[K, V] => parentPos - 1
      case ul: UpperBoundLeaf[K, V] => {
        parentPos - 1
      }
    }
  }

  private[intervaltree]
  def posOfRight[K, V](tree: IntervalTree[K, V], parentPos: Int): Int = {
    tree match {
      case empty: EmptyNode[K, V] => parentPos + 1
      case InternalNode(_, _, left, _) => {
        parentPos + left.size + 1
      }
      case UpperBoundInternalNode(_, _, left) => parentPos + 1
      case l: Leaf[K, V] => parentPos + 1
      case ul: UpperBoundLeaf[K, V] => {
        parentPos + 1
      }
    }
  }

  def buildOne[K: Ordering, V](
        xLow: K, xUpp: K, f: V
  )(implicit ord: Ordering[K]): IntervalTree[K, V] = {
    if (ord.lteq(xLow, xUpp)) {
      new UpperBoundLeaf[K, V](xLow, xUpp, f)
    }
    else EmptyNode[K, V]()
  }

  def valueAt[K: Ordering, V](x: K, tree: IntervalTree[K, V]): Option[V] = {
      find(x, tree) match{
        case empty: EmptyNode[K, V] => None
        case nonEmpty: NonEmptyITree[K, V, _] => Some(nonEmpty.v)
      }
  }

  def unsafeValueAt[K: Ordering, V](x: K, tree: IntervalTree[K, V]): V = {
    find(x, tree) match {
      case empty: EmptyNode[K, V] => ???
      case nonEmpty: NonEmptyITree[K, V, _] => nonEmpty.v
    }
  }

  @tailrec
  def find[K: Ordering, V](x: K, tree: IntervalTree[K, V]): IntervalTree[K, V] = {
    tree match {
      case empty: EmptyNode[K, V] => empty
      case internal: InternalNode[K, V] => {
        if (internal.contains(x)) internal
        else if (internal.upperThan(x)) find(x, internal.left)
        else find(x, internal.right)
      }
      case upperInternal: UpperBoundInternalNode[K, V] => {
        if (upperInternal.contains(x)) upperInternal
        else if (upperInternal.upperThan(x)) find(x, upperInternal.left)
        else EmptyNode[K, V]
      }
      case leaf: Leaf[K, V] => {
        if (leaf.contains(x)) leaf
        else EmptyNode[K, V]
      }
      case leaf: UpperBoundLeaf[K, V] => {
        if (leaf.contains(x)) leaf
        else EmptyNode[K, V]
      }
    }
}

  implicit val bifunctor: Bifunctor[IntervalTree] =
    new Bifunctor[IntervalTree] {
    override def bimap[A, B, C, D](
                   fab: IntervalTree[A, B])(
      f: A => C, g: B => D): IntervalTree[C, D] = {
      val size = fab.size

      val morphed =
        fab.iterator.map{
          case ((k1, k2), v1) => ((f(k1), f(k2)), g(v1))
        }

      buildLeft(morphed, size)
    }
  }

  implicit def showIntervalTree[K, V](
    implicit showNonEmpty: Show[NonEmptyITree[K, V, _]]): Show[IntervalTree[K, V]] =
    (iTree: IntervalTree[K, V]) => {
      iTree match {
        case empty: EmptyNode[K, V] => ""
        case nonEmpty: NonEmptyITree[K, V, _] => {
          showNonEmpty.show(nonEmpty)
        }
      }
  }

  implicit def showNonEmptyIntervalTree[K, V](
    implicit showClosedInterval: Show[Interval[K, Closed, Closed]],
             showClosedOpenInterval: Show[Interval[K, Closed, Open]]
    ): Show[NonEmptyITree[K, V, _]] =
    new Show[NonEmptyITree[K, V, _]]{
      override def show(t: NonEmptyITree[K, V, _]): String = {
        val maybeEmpty = showIntervalTree[K, V](this)

        t match {
          case leaf: Leaf[K, V] =>
            s"${showClosedOpenInterval.show(leaf.interval)}: ${t.v.toString}\r\n"
          case uLeaf: UpperBoundLeaf[K, V] => {
            s"${showClosedInterval.show(uLeaf.interval)}: ${t.v.toString}"
          }
          case internal: InternalNode[K, V] => {

            val left = maybeEmpty.show(internal.left)

            val mid =
              s"${showClosedOpenInterval.show(internal.interval)}: ${t.v.toString}\r\n"

            val right = maybeEmpty.show(internal.right)

            left + mid + right
          }
          case uInternal: UpperBoundInternalNode[K, V] => {

            val left = show(uInternal)

            val mid =
              s"${showClosedInterval.show(uInternal.interval)}: ${t.v.toString}"

            left + mid
          }
        }
      }
    }


  implicit def showEmptyIntervalTree[K, V]: Show[EmptyNode[K, V]] =
    (value: EmptyNode[K, V]) =>  ""



  final def buildLeft[K, V](vals: Iterator[((K, K), V)],
                            size: Int): IntervalTree[K, V] = {

      size match {
        case 1 => {
          new Leaf(vals.next())
        }
        case 2 => {
          val leafData = vals.next()
          new InternalNode(vals.next(), new Leaf(leafData), EmptyNode[K, V]())
        }
        case 3 => {
          val left = vals.next()
          val mid = vals.next()
          val right = vals.next()

          new InternalNode(mid, new Leaf(left), new Leaf(right)
          )
        }
        case s => {
          val leftIndex =
            if(s % 2 == 0) (size + 1) / 2
            else s / 2
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val mid = vals.next()
          val right = buildRight(vals, rightSize)
          new InternalNode[K, V](mid, left, right)
        }
      }
  }
  final def buildRight[K, V](vals: Iterator[((K, K), V)],
                                       size: Int): IntervalTree[K, V] = {
      size match {
        case 0 => {
          EmptyNode[K, V]
        }
        case 1 => {
          val next = vals.next()
          if (vals.hasNext) new Leaf(next)
          else new UpperBoundLeaf(next)
        }
        case 2 => {
          val mid = vals.next()
          new InternalNode[K, V](mid, EmptyNode[K, V](), buildRight(vals, 1))
        }
        case 3 => {
          val left = vals.next()
          val mid = vals.next()
          new InternalNode(mid, new Leaf(left), buildRight(vals, 1))
        }

        case s => {

          val leftIndex =
            if(size % 2 == 0) (size - 1) / 2
            else size / 2
          
          val rightSize = size - leftIndex - 1
          val left = buildLeft(vals, leftIndex)
          val mid = vals.next()
          val right = buildRight(vals, rightSize)
          new InternalNode[K, V](mid, left, right)
        }
      }
  }

  import cats._
  final def subIntervalFold[K, V, R](
         tree: IntervalTree[K, V],
         low: K,
         upp: K,
         f: Function3[K, K, V, R]
    )(implicit R: Monoid[R], ord: Ordering[K]): R = {
      tree match {
        case i @ InternalNode(_, v, left, right) => {

          val l =
            if (i.upperThan(low)) subIntervalFold(left, low, i.low, f)
            else R.empty

          val r =
            if (i.lowerThan(upp)) subIntervalFold(right, i.upp, upp, f)
            else R.empty

          val c = f(
              ord.max(low, i.low),
              ord.min(upp, i.upp),
              v
          )

          R.combine(l, R.combine(c, r))
        }
        case i @ UpperBoundInternalNode(_, v, left) => {

          val l =
            if (i.upperThan(low)) subIntervalFold(left, low, i.low, f)
            else R.empty

          val c = f(
            ord.max(low, i.low),
            ord.min(upp, i.upp),
            v
          )

          R.combine(l, c)
        }
        case l @ Leaf(_, v) => {

          f(
            ord.max(l.low, low),
            ord.min(l.upp, upp),
            v
          )
        }
        case l @ UpperBoundLeaf(_, v) => {

          f(
            ord.max(l.low, low),
            ord.min(l.upp, upp),
            v
          )
        }
        case e: EmptyNode[K, V] => R.empty
      }
  }


}