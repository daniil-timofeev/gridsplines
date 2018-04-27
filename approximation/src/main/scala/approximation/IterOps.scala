package approximation

abstract class IterOps {

  def posAtLayer: Int
  def layer: Int
  def hasNext: Boolean
  def hasPrev: Boolean
  def prev: Int
  def hasTwoNext: Boolean
  def next: Int
  def hasNextLayer: Boolean
  def nextLayer: Int
  def reset: Unit


}
