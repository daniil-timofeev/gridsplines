package piecewise
import org.specs2._
import piecewise.intervaltree._
class IntervalTreeSpecs extends Specification{def is = s2"""
    Build empty interval tree ${buildEmpty}
    Build interval tree with one mebmer ${buildOne}
    Build interval tree with two members ${buildTwo}
    Build interval tree with three members ${buildThree}
    Build interval tree with four members ${buildFour}
    Build interval tree with five members ${buildFive}
    Build interval tree with six members ${buildSix}
    Build interval tree with seven members ${buildSeven}
    Build interval tree with eight members ${buildEight}
    Build interval tree with nine members ${buildNine}
    Build interval tree with ten members ${buildTen}
    Ten integers fold ${fold}
  """

  def iterator: Iterator[((Double, Double), Int)] = {
    Iterator.iterate((0.0, 1.0))(t => {val (x0, x1) = t; (x0 + 1.0, x1 + 1.0)})
      .zip(
        Iterator.iterate(0)(i => i + 1)
    )
  }

  def buildEmpty = {
    val tree = IntervalTree.buildRight(iterator.take(0), 0)
    val iter = tree.iterator

    (tree.size must_== 0) and
    (iter.size must_== 0)
  }

  def buildOne = {
    val tree = IntervalTree.buildRight(iterator.take(1), 1)
    val iter = tree.iterator

    (tree.size must_== 1) and
    (iter.size must_== 1)
  }

  def buildTwo = {
    val tree = IntervalTree.buildRight(iterator.take(2), 2)
    val iter = tree.iterator

    (tree.size must_== 2) and
    (iter.size must_== 2)
  }

  def buildThree = {
    val tree = IntervalTree.buildRight(iterator.take(3), 3)
    val iter = tree.iterator

    (tree.size must_== 3) and
    (iter.size must_== 3)
  }

  def buildFour = {
    val tree = IntervalTree.buildRight(iterator.take(4), 4)
    val iter = tree.iterator

    (tree.size must_== 4) and
    (iter.size must_== 4)
  }

  def buildFive = {
    val tree = IntervalTree.buildRight(iterator.take(5), 5)
    val iter = tree.iterator

    (tree.size must_== 5) and
    (iter.size must_== 5)

  }

  def buildSix = {
    val tree = IntervalTree.buildRight(iterator.take(6), 6)
    val iter = tree.iterator

    (tree.size must_== 6) and
    (iter.size must_== 6)
  }

  def buildSeven = {
    val tree = IntervalTree.buildRight(iterator.take(7), 7)
    val iter = tree.iterator

    (tree.size must_== 7) and
    (iter.size must_== 7)
  }

  def buildEight = {
    val tree = IntervalTree.buildRight(iterator.take(8), 8)
    val iter = tree.iterator

    (tree.size must_== 8) and
    (iter.size must_== 8)
  }

  def buildNine = {
    val tree = IntervalTree.buildRight(iterator.take(9), 9)
    val iter = tree.iterator

    (tree.size must_== 9) and
    (iter.size must_== 9)
  }

  def buildTen = {
    val tree = IntervalTree.buildRight(iterator.take(10), 10)
    val iter = tree.iterator

    (tree.size must_== 10) and
    (iter.size must_== 10)
  }


  def fold = {
    val tree = IntervalTree.buildRight(iterator.take(10), 10)

    val sum = iterator.take(10).map(_._2).sum

    val calcSum =
      IntervalTree.subIntervalFold(tree, 0.0, 11.0, (l: Double, u: Double, v: Int) => v)

    calcSum must_== sum
    }

}

