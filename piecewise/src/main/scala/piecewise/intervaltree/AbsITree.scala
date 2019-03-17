package piecewise.intervaltree

import cats.{Eval, Functor, Group, Monoid, Order, Show}
import piecewise.intervaltree.Interval.Empty

import scala.annotation.tailrec
import scala.collection.Iterator
import scala.languageFeature.existentials._
import scala.languageFeature.higherKinds._

@specialized(Double, Float)
abstract class AbsITree[+K, +V] {
  def size: Int
  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = !isEmpty
}
abstract class NonEmptyInt[+K, +V] extends AbsITree[K, V]{
  type U <: BoundType

  def interval: Interval[K, Closed, U]
  def value: V
  def _1: Interval[K, Closed, U] = interval
  def _2: V = value
  def unapplyNode: (Interval[K, Closed, U], V) = (_1, _2)
  def foldValues[R](f: (K, K, V) => R): R =
    f(interval.lower.bound, interval.upper.bound, value)

  def foldViewValues[R, K0 >: K](low: K0, upp: K0)(f: (K0, K0, V) => R)(
    implicit ord: Order[K0], g: Group[K0], m: Monoid[R]): R = {
    Interval.foldWithFilter(low, upp)(interval, value)(f)
  }

  def low: K = AbsITree.leastK(this).get
  def upp: K = AbsITree.greatestK(this).get
}

case object EmptyInt extends AbsITree[Nothing, Nothing] {
  val size = 0
}
object AbsITree {

  final
  def singleton[K: Order, V](xLow: K, xUpp: K, v: V): AbsITree[K, V] =
    singleton(Csd.pure(xLow, xUpp), v)

  final
  def singleton[K, Order, V](interval: AbsInterval[K], v: V): AbsITree[K, V] = {
    interval match {
      case Interval.Empty => EmptyInt
      case nonEmpty: Csd[K] => new UpBoundLeaf(nonEmpty, v)
      case _ => EmptyInt
    }
  }

  final
  def leaf[K: Order, V](xLow: K, xUpp: K, f: V): AbsITree[K, V] =
  leaf(CsdOpn.pure(xLow, xUpp), f)

  final
  def leaf[K: Order, V](interval: AbsInterval[K], v: V): AbsITree[K, V] = {
    interval match {
      case Interval.Empty => EmptyInt
      case nonEmpty: CsdOpn[K] => new Leaf[K, V](nonEmpty, v)
      case _ => EmptyInt
    }
  }

  final
  def inNode[K: Order, V](interval: AbsInterval[K], v: V,
                          left: AbsITree[K, V],
                          right: AbsITree[K, V]): AbsITree[K, V] =
  interval match {
    case Empty => nonEmpty(left, right)
    case nonEmpty: CsdOpn[K] => {
      right match {
        case EmptyInt => upBoundInNode(nonEmpty.toClosed, v, left)
        case nEmptyRight => new InNode(nonEmpty, v, left, nEmptyRight)
      }
    }
  }

  final def build[K, V](vals: Iterator[((K, K), V)], size: Int)(
    implicit ord: Order[K]): AbsITree[K, V] = {
    buildRight(vals, size).value
  }

  final def build[K: Order, V](vals: List[((K, K), V)]): AbsITree[K, V] =
    build(vals.iterator, vals.size)

  private
  def nonEmpty[K, V](left: AbsITree[K, V], right: AbsITree[K, V]): AbsITree[K, V] =
  left match {
    case EmptyInt => right
    case nonEmpty => nonEmpty
  }

  final
  def upBoundInNode[K: Order, V](interval: AbsInterval[K],
  v: V, left: AbsITree[K, V]): AbsITree[K, V] =
  interval match {
    case Empty => left
    case nonEmpty: Csd[K] => left match {
      case EmptyInt => UpBoundLeaf(nonEmpty, v)
      case _ => UpBoundInNode(nonEmpty, v, left)
    }
  }


  final def buildLeft[K, V](vals: Iterator[((K, K), V)],
                            size: Int): Eval[AbsITree[K, V]] =
    size match {
      case 1 => Eval.now(Leaf(vals.next()))
      case 2 => Eval.now{
        val leafData = vals.next()
        InNode(vals.next(), Leaf(leafData), EmptyInt)
      }
      case 3 => Eval.now {
        val left = vals.next()
        val mid = vals.next()
        val right = vals.next()
        InNode(mid, Leaf(left), Leaf(right))
      }
      case s => for {
        leftIndex <- Eval.now(
          if(s % 2 == 0) (size + 1) / 2
          else s / 2
        )
        left <- Eval.defer(buildLeft(vals, leftIndex))
        mid <- Eval.now(vals.next())
        right <- Eval.defer{
          val rightSize = size - leftIndex - 1
          buildRight(vals, rightSize)
        }
      } yield InNode(mid, left, right)
    }

  final def buildRight[K, V](vals: Iterator[((K, K), V)],
                             size: Int): Eval[AbsITree[K, V]] =
    size match {
      case 0 => Eval.now(EmptyInt)
      case 1 =>
        Eval.now{
          val next = vals.next()
          if (vals.hasNext) Leaf(next)
          else UpBoundLeaf(next)
        }
      case 2 => for {
        mid <- Eval.now(vals.next())
        right <- buildRight(vals, 1)
      } yield InNode(mid, EmptyInt, right)
      case 3 => for {
        left <- Eval.now(Leaf(vals.next()))
        mid <- Eval.now(vals.next())
        right <- Eval.defer(buildRight(vals, 1))
      } yield InNode(mid, left, right)
      case s => for {
        leftIndex <- Eval.now{if(size % 2 == 0) (size - 1) / 2
                              else size / 2}
        left <- Eval.defer(buildLeft(vals, leftIndex))
        mid <- Eval.now(vals.next())
        right <- Eval.defer{
          val rightSize = size - leftIndex - 1
          buildRight[K, V](vals, rightSize)
        }
      } yield InNode(mid, left, right)
    }

  final def prepend[K: Order, V](mostLeftK: K,
                                 mostLeftV: V)(ins: AbsITree[K, V]): AbsITree[K, V] =
    ins match {
      case EmptyInt => throw new RuntimeException("Could not prepend empty tree")
      case xs: NonEmptyInt[K, V] =>
        goPrepend(mostLeftK, mostLeftV, xs.interval.lower.bound)(xs)
    }


  private final def goPrepend[K, V](mostLeftK: K,
                                    mostLeftV: V,
                                    mostLeft: K)(
    ins: AbsITree[K, V])(implicit ord: Order[K]): AbsITree[K, V] =
    ins match {
      case InNode(i, v, l, r) => InNode(i, v,
        goPrepend(mostLeftK, mostLeftV, i.lower.bound)(l), r)
      case UpBoundInNode(i, v, l) =>
        UpBoundInNode(i, v, goPrepend(mostLeftK, mostLeftV, i.lower.bound)(l))
      case EmptyInt =>
        if (ord.lt(mostLeftK, mostLeft))
          new Leaf(CsdOpn.pure.unsafe(mostLeftK, mostLeft), mostLeftV)
        else throw new IllegalArgumentException(
          "`MostLeftK` must be less then left interval bound")
      case Leaf(i, v) =>
        InNode(i, v,
          goPrepend(mostLeftK, mostLeftV, i.lower.bound)(EmptyInt), EmptyInt)
      case UpBoundLeaf(i, v) =>
        UpBoundInNode(i, v,
          goPrepend(mostLeftK, mostLeftV, i.lower.bound)(EmptyInt))
    }

  final def append[K: Order, V](mostRightK: K,
                                mostRightV: V
                               )(ins: AbsITree[K, V]): AbsITree[K, V] = {
    ins match {
      case EmptyInt =>
        throw new RuntimeException("Could not append to empty tree")
      case xs: NonEmptyInt[K, V] =>
        goAppend(mostRightK, mostRightV, xs.interval.upper.bound)(xs)
    }
  }

  private final def goAppend[K, V](mostRightK: K,
                                   mostRightV: V,
                                   mostRight: K)(ins: AbsITree[K, V])(
    implicit ord: Order[K]): AbsITree[K, V] =
    ins match {
      case InNode(i, v, l, r) =>
        InNode(i, v, l, goAppend(mostRightK, mostRightV, i.upper.bound)(r))
      case UpBoundLeaf(i, v) => InNode(i.toLeftClosedRightOpen, v, EmptyInt,
        goAppend(mostRightK, mostRightV, i.upper.bound)(EmptyInt))
      case EmptyInt =>
        if (ord.lt(mostRight, mostRightK))
          new UpBoundLeaf(mostRight, mostRightK, mostRightV)
        else throw new RuntimeException()
      case UpBoundInNode(i, v, l) =>
        InNode(i.toLeftClosedRightOpen, v, l,
          goAppend(mostRightK, mostRightV, i.upper.bound)(EmptyInt)
        )
      case Leaf(_, _) =>
        throw new IllegalArgumentException(
          "`MostRightK` must be upper then right interval bound"
        )
    }

  final
  def slice[K: Order, V](l: K, u: K)(ins: AbsITree[K, V]): Eval[AbsITree[K, V]] = {
    ins match {
      case EmptyInt => Eval.now(EmptyInt)
      case Leaf(i: CsdOpn[K], v: V) => Eval.now(leaf(Interval.slice(i)(l, u), v))
      case UpBoundLeaf(i: Csd[K], v: V) =>
        Eval.now(singleton(Interval.slice(i)(l, u), v))
      case InNode(i: CsdOpn[K], v: V, left, right) =>
        for {
          newL <- Eval.defer(slice(l, u)(left))
          newR <- Eval.defer(slice(l, u)(right))
        } yield inNode(Interval.slice(i)(l, u), v, newL, newR)
      case UpBoundInNode(i: Csd[K], v: V, left) =>
        Eval.defer(slice(l, u)(left)).map(newLeft =>
          upBoundInNode(Interval.slice(i)(l, u), v, newLeft)
        )
    }
  }

  @tailrec final
  def leastK[K, V](ins: AbsITree[K, V]): Option[K] =
    ins match {
      case EmptyInt => None
      case Leaf(i, _) => Some(i.lower.bound)
      case UpBoundLeaf(i, _) => Some(i.lower.bound)
      case InNode(i ,_ , l, _) =>
        l match {
          case EmptyInt => Some(i.lower.bound)
          case nonEmpty => leastK(nonEmpty)
        }
      case UpBoundInNode(i, _, l) =>
        l match {
          case EmptyInt => Some(i.lower.bound)
          case nonEmpty => leastK(nonEmpty)
        }
  }

  @tailrec
  final def greatestK[K, V](ins: AbsITree[K, V]): Option[K] =
    ins match {
      case EmptyInt => None
      case UpBoundLeaf(i, _) => Some(i.lower.bound)
      case UpBoundInNode(i, _, _) => Some(i.upper.bound)
      case InNode(_, _, _, right) => greatestK(right)
      case Leaf(_, _) => throw new RuntimeException(
        "Interval tree implementation error"
      )
  }

  @tailrec
  final def leastV[K, V](ins: AbsITree[K, V]): Option[V] =
    ins match {
      case InNode(_, v, EmptyInt, _) => Some(v)
      case UpBoundInNode(_, v, EmptyInt) => Some(v)
      case InNode(_, _, l, _) => leastV(l)
      case UpBoundInNode(_, _, l) => leastV(l)
      case Leaf(_, v) => Some(v)
      case UpBoundLeaf(_, v) => Some(v)
      case EmptyInt => None
    }

  def greatestV[K, V](ins: AbsITree[K, V]): Option[V] =
    ins match {
      case InNode(_, _, _, r) => greatestV(r)
      case UpBoundInNode(_, v, _) => Some(v)
      case InNode(_, _, _, r) => greatestV(r)

      case Leaf(_, v) => throw new RuntimeException(
        "greatest node class must has prefix `UpBound`")
      case UpBoundLeaf(_, v) => Some(v)
      case EmptyInt => None
    }

  final def wholeInterval[K, V](ins: AbsITree[K, V]): AbsInterval[K] = {
    val optInt = for {
      l <- leastK(ins)
      u <- greatestK(ins)
    } yield Csd.pure.unsafe(l, u)
    optInt.getOrElse(Empty)
  }

  @specialized(Float, Double)
  @tailrec
  final def find[K: Order, V](x: K)(ins: AbsITree[K, V]): AbsITree[K, V] =
    ins match {
      case EmptyInt => EmptyInt
      case l @ Leaf(i: CsdOpn[K], _) =>
        if (Interval.contains(i)(x)) l else EmptyInt
      case l @ UpBoundLeaf(i: Csd[K], _) =>
        if (Interval.contains(i)(x)) l else EmptyInt
      case n @ InNode(i: CsdOpn[K], _, l, r) =>
        if (Interval.contains(i)(x)) n
        else {
          if (Interval.isUpperThan(i)(x)) find(x)(l)
          else find(x)(r)
        }
      case n @ UpBoundInNode(i: Csd[K], _, l) =>
        if (Interval.contains(i)(x)) n
        else if (Interval.isUpperThan(i)(x)) find(x)(l)
        else EmptyInt
    }

  @specialized(Float, Double)
  final def find[K: Order, V, R, R1 >: R](x: K, ins: AbsITree[K, V])(
    empty: => R)(
    nonEmpty: NonEmptyInt[K, V] => R1): R1 = find(x)(ins) match {
    case EmptyInt => empty
    case x: NonEmptyInt[K, V] => nonEmpty(x)
  }

  @specialized(Float, Double)
  final def contains[K: Order, V](x: K)(ins: AbsITree[K, V]): Boolean =
    ins match {
      case EmptyInt => false
      case l @ Leaf(i: CsdOpn[K], _) => Interval.contains(i)(x)
      case l @ UpBoundLeaf(i: Csd[K], _) => Interval.contains(i)(x)
      case n @ InNode(i: CsdOpn[K], _, l, r) =>
        Interval.contains(i)(x) ||
        (Interval.isLowerThan(i)(x) && contains(x)(r)) ||
        (Interval.isUpperThan(i)(x) && contains(x)(l))
      case n @ UpBoundInNode(i: Csd[K], _, l) =>
        Interval.contains(i)(x) ||
          (Interval.isUpperThan(i)(x) && contains(x)(l))
    }

  final
  def sliceLower[K, V](newLow: K)(ins: AbsITree[K, V])(implicit ord: Order[K]): Eval[AbsITree[K, V]] =
    Eval.now(
      AbsITree.build(AbsITree.toArray(ins).iterator.map{
        case (i, v) => (i.unapply, v)
      }.collect{
        case a @ ((l, u), v) if ord.gteqv(l, newLow) => a
        case ((l, u), v) if ord.lt(l, newLow) => ((newLow, u), v)
      }.toList
    ))

  final
  def sliceUpper[K, V](newUpp: K)(ins: AbsITree[K, V])(implicit ord: Order[K]): Eval[AbsITree[K, V]] =
    Eval.now(
      AbsITree.build(AbsITree.toArray(ins).iterator.map{
        case (i, v) => (i.unapply, v)
      }.collect{
        case a @ ((l, u), v) if ord.lteqv(u, newUpp) => a
        case ((l, u), v) if ord.gt(u, newUpp) && ord.lteqv(l, newUpp) => ((l, newUpp), v)
      }.toList
    ))



  import cats.syntax.show._

  implicit def showITree[K, V](
    implicit showCsdInterval: Show[CsdOpn[K]],
             showCsdOpnInterval: Show[Csd[K]],
             showValue: Show[V]
  ): Show[AbsITree[K, V]] = Show.show((iTree: AbsITree[K, V]) => {
    toArray(iTree) match {
      case emptyArr if emptyArr.isEmpty => ""
      case nonEmpty => {
        nonEmpty.iterator.map{
          case (i: Csd[K], v: V) => showStructure(i, v)
          case (i: CsdOpn[K], v: V) => showStructure(i, v)
        }.reduce(_ + "; " + _)
      }
    }})

  private final def showStructure[K, V](key: K, value: V)(
    implicit sK: Show[K],
             sV: Show[V]): String =
    s"""${key.show}: ${value.show}"""


  def toArray[K, V](iTree: AbsITree[K, V]): Array[(Interval[K, Closed, _], V)] =
    iTree match {
      case EmptyInt => Array.empty
      case leaf: AbsLeaf[K, V] => Array(leaf.unapplyNode)
      case node: AbsInNode[K, V] => {
        val arr = new Array[(Interval[K, Closed, _], V)](node.size)
        val pos = node.left.size
        goArray(node, arr, pos).value
      }
  }

  private
  final def goArray[K, V](arg: AbsInNode[K, V],
                          arr: Array[(Interval[K, Closed, _], V)],
                          pos: Int): Eval[Array[(Interval[K, Closed, _], V)]] = {

    def goLeft(left: AbsITree[K, V],
               arr: Array[(Interval[K, Closed, _], V)]
              ): Eval[Array[(Interval[K, Closed, _], V)]] =
      left match {
        case inNode: AbsInNode[K, V] => Eval.defer{
          val leftIndex = pos - inNode.right.size - 1
          goArray(inNode, arr, leftIndex)
       }
        case leaf: AbsLeaf[K, V] => Eval.now{
          val leftIndex = pos - 1
          arr.update(leftIndex, leaf.unapplyNode)
          arr
       }
        case EmptyInt => Eval.now(arr)
      }

    def goRight(right: AbsITree[K, V],
                arr: Array[(Interval[K, Closed, _], V)]
               ): Eval[Array[(Interval[K, Closed, _], V)]] =
     right match {
        case inNode: AbsInNode[K, V] => Eval.defer{
          val rightIndex = pos + inNode.left.size + 1
          goArray(inNode, arr, rightIndex)
        }
        case leaf: AbsLeaf[K, V] => Eval.now{
          val rightIndex = pos + 1
          arr.update(rightIndex, leaf.unapplyNode)
          arr
        }
        case EmptyInt => Eval.now(arr)
     }

   for {
     l <- goLeft(arg.left, arr)
     c <- Eval.now{l.update(pos, arg.unapplyNode); l}
     r <- goRight(arg.right, c)
   } yield r
  }

  def rebuild[K: Order, V](ins: AbsITree[K, V]): AbsITree[K, V] = {
    val array = toArray(ins)
    val size = array.length
    build(array.iterator.map(x => x.copy(x._1.unapply)), size)
  }

  def valueAt[K: Order, V](x: K, tree: AbsITree[K, V]): Option[V] = {
    find(x)(tree) match{
      case EmptyInt => None
      case nonEmpty: NonEmptyInt[K, V]  => Some(nonEmpty.value)
    }
  }

  def unsafeValueAt[K: Order: Show, V](x: K, tree: AbsITree[K, V]): V = {
    find(x)(tree) match {
      case EmptyInt =>
        throw new RuntimeException(
          s"Interval tree does not defined at ${x.show}." +
            s" Try values within bounds: ${Interval.show(wholeInterval(tree))}"
        )
      case nonEmpty: NonEmptyInt[K, V] => nonEmpty.value
    }
  }

  @specialized(Double, Float)
  def viewFold[K: Order: Group, V, R](low: K, upp: K)(
                               tree: AbsITree[K, V])(
                               fi: (K, K, V) => R)(implicit m: Monoid[R]): R =
    tree match {
      case self @ InNode(i: CsdOpn[K], _, l, r) => {
        Interval.relateTo(i)(low) match {
          case LT => Interval.relateTo(i)(upp) match {
            case LT => viewFold(low, upp)(r)(fi)
            case EQ => m.empty
            case GT => m.empty
          }
          case EQ =>
            m.combine(self.foldViewValues(low, upp)(fi),
                      viewFoldRight(upp)(r)(fi))
          case GT => Interval.relateTo(i)(upp) match {
            case GT => viewFold(low, upp)(l)(fi)
            case EQ =>
              m.combine(viewFoldLeft(low)(l)(fi), self.foldViewValues(low, upp)(fi))
            case LT => m.combine(
              m.combine(viewFoldLeft(low)(l)(fi), self.foldViewValues(low, upp)(fi)),
              viewFoldRight(upp)(r)(fi))
          }
        }
      }
      case leaf: AbsLeaf[K, V] => leaf.foldViewValues(low, upp)(fi)
      case self @ UpBoundInNode(i: Csd[K], _, l) =>
        Interval.relateTo(i)(upp) match {
          case LT => Interval.relateTo(i)(upp) match {
            case LT => m.empty
            case EQ => self.foldViewValues(low, upp)(fi)
            case GT => m.combine(self.foldValues(fi), viewFoldAll(l)(fi))
          }
          case EQ => Interval.relateTo(i)(upp) match {
            case LT => m.empty
            case EQ => self.foldViewValues(low, upp)(fi)
            case GT => m.combine(
              self.foldViewValues(low, upp)(fi),
              viewFoldLeft(low)(l)(fi)
            )
          }
          case GT => Interval.relateTo(i)(low) match {
            case LT => m.empty
            case EQ => m.empty
            case GT => viewFold(low, upp)(l)(fi)
          }
        }
      case EmptyInt => m.empty
    }

  @specialized(Double, Float)
  def viewFoldLeft[K: Order: Group, V, R](low: K)(
    tree: AbsITree[K, V])(
    fi: (K, K, V) => R)(implicit m: Monoid[R]): R =
    tree match {
      case self @ InNode(i: CsdOpn[K], _, l, r) =>
        Interval.relateTo(i)(low) match {
          case LT => viewFoldLeft(low)(r)(fi)
          case EQ =>
            m.combine(self.foldViewValues(low, i.upper.bound)(fi),
                      viewFoldAll(r)(fi))
          case GT => {
            val after = viewFoldLeft(low)(l)(fi)
            val before = m.combine(self.foldValues(fi), viewFoldAll(r)(fi))
            m.combine(before, after)
          }
        }
      case self @ UpBoundInNode(i: Csd[K], _, l) =>
        Interval.relateTo(i)(low) match {
          case LT => m.empty
          case EQ => self.foldViewValues(low, i.upper.bound)(fi)
          case GT => m.combine(self.foldValues(fi), viewFoldLeft(low)(l)(fi))
      }
      case self @ Leaf(i: CsdOpn[K], _) =>
         self.foldViewValues(low, i.upper.bound)(fi)
      case self @ UpBoundLeaf(i: Csd[K], _) =>
        self.foldViewValues(low, i.upper.bound)(fi)
      case EmptyInt => m.empty
    }

  @specialized(Double, Float)
  def viewFoldRight[K: Order: Group, V, R](upp: K)(tree: AbsITree[K, V])(
    fi: (K, K, V) => R)(implicit m: Monoid[R]): R =
       tree match {
         case self @ InNode(i: CsdOpn[K], _, l, r) =>
           Interval.relateTo(i)(upp) match {
             case LT => {
               val before = m.combine(self.foldValues(fi), viewFoldAll(l)(fi))
               val after = viewFoldRight(upp)(r)(fi)
               m.combine(before, after)
             }
             case EQ =>
               m.combine(
                 self.foldViewValues(i.lower.bound, upp)(fi),
                 viewFoldAll(l)(fi)
               )
             case GT => viewFoldRight(upp)(l)(fi)
           }
         case self @ Leaf(i: CsdOpn[K], _) =>
           self.foldViewValues(i.lower.bound, upp)(fi)
         case self @ UpBoundLeaf(i: Csd[K], _) =>
           self.foldViewValues(i.lower.bound, upp)(fi)
         case self @ UpBoundInNode(i: Csd[K], _, l) =>
           Interval.relateTo(i)(upp) match {
             case LT => m.combine(self.foldValues(fi), viewFoldAll(l)(fi))
             case EQ => m.combine(
               self.foldViewValues(i.lower.bound, upp)(fi),
               viewFoldAll(l)(fi)
             )
             case GT => viewFoldRight(upp)(l)(fi)
           }
         case EmptyInt => m.empty
       }

  @specialized(Double, Float)
  def viewFoldAll[K: Order, V, R](tree: AbsITree[K, V])(
    fi: (K, K, V) => R)(implicit m: Monoid[R]): R =
    tree match {
      case self @ InNode(_, _, l, r) => {
        val node = self.foldValues(fi)
        val leafs = m.combine(viewFoldAll(l)(fi), viewFoldAll(r)(fi))
        m.combine(node, leafs)
      }
      case leaf: Leaf[K, V] => leaf.foldValues(fi)
      case self @ UpBoundInNode(_, _, l) =>
        m.combine(self.foldValues(fi), viewFoldAll(l)(fi))
      case upBoundLeaf: UpBoundLeaf[K, V] => upBoundLeaf.foldValues(fi)
      case EmptyInt => m.empty
    }

  implicit def functorV[K]: Functor[({type L[A] = AbsITree[K, A]})#L] =
    new Functor[({type L[A] = AbsITree[K, A]})#L] {
      private final
      def evalMap[A, B](fa: AbsITree[K, A])(f: A => B): Eval[AbsITree[K, B]] =
        fa match {
        case InNode(i, v, l: AbsITree[K, A], r: AbsITree[K, A]) => for {
          fl <- Eval.defer(evalMap(l)(f))
          fr <- Eval.defer(evalMap(r)(f))
        } yield InNode(i, f(v), fl, fr)
        case UpBoundInNode(i, v, l: AbsITree[K, A]) =>
          Eval.defer(evalMap(l)(f)).map(fl => UpBoundInNode(i, f(v), fl))
        case Leaf(i, v) => Eval.now(Leaf(i, f(v)))
        case UpBoundLeaf(i, v) => Eval.now(UpBoundLeaf(i, f(v)))
        case EmptyInt => Eval.now(EmptyInt)
      }
      override def map[A, B](fa: AbsITree[K, A])(f: A => B): AbsITree[K, B] =
        evalMap[A, B](fa)(f).value
  }


}