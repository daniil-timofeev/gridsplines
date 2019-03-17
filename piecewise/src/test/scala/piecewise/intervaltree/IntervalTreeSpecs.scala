package piecewise
package intervaltree

import cats.instances.double._
import org.specs2._

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
      .zip(Iterator.iterate(0)(i => i + 1))
  }

  def buildEmpty = {
    val tree = AbsITree.build(iterator.take(0), 0)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 0) and
    (arr.length must_== 0)
  }

  def buildOne = {
    val tree = AbsITree.build(iterator.take(1), 1)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 1) and
    (arr.length must_== 1)
  }

  def buildTwo = {
    val tree = AbsITree.build(iterator.take(2), 2)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 2) and
    (arr.length must_== 2)
  }

  def buildThree = {
    val tree = AbsITree.build(iterator.take(3), 3)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 3) and
    (arr.length must_== 3)
  }

  def buildFour = {
    val tree = AbsITree.build(iterator.take(4), 4)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 4) and
    (arr.length must_== 4)
  }

  def buildFive = {
    val tree = AbsITree.build(iterator.take(5), 5)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 5) and
    (arr.length must_== 5)

  }

  def buildSix = {
    val tree = AbsITree.build(iterator.take(6), 6)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 6) and
    (arr.length must_== 6)
  }

  def buildSeven = {
    val tree = AbsITree.build(iterator.take(7), 7)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 7) and
    (arr.length must_== 7)
  }

  def buildEight = {
    val tree = AbsITree.build(iterator.take(8), 8)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 8) and
    (arr.length must_== 8)
  }

  def buildNine = {
    val tree = AbsITree.build(iterator.take(9), 9)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 9) and
    (arr.length must_== 9)
  }

  def buildTen = {
    val tree = AbsITree.build(iterator.take(10), 10)
    val arr = AbsITree.toArray(tree)

    (tree.size must_== 10) and
    (arr.length must_== 10)
  }


  def fold = {
      pending
    }

}

