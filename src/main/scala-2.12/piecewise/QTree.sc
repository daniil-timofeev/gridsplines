import com.twitter.algebird._

val q = QTree(1.0)

// make a list of data lower numbers
val list = List(1, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 6, 7, 8).map(_ toDouble)

// qtree yL k
val k = 5

// create semigroup object for qtree
val qtSemigroup = new QTreeSemigroup[Double](5)

val qq = QTree(1.0) :: QTree(2.0) :: QTree(4.0) :: QTree(5.0):: Nil

val res = qq.reduce{qtSemigroup.plus(_, _)}

res.range
res.quantileBounds(0.8)
res.rangeCountBounds(0.5, 4.0)
res.lowerBound
res.upperBound




// map list data into qtree
val buildQTree = list.map { QTree(_) }.reduce { qtSemigroup.plus(_, _) }

// make a second list of data higher
val list2 = List(2, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 6, 7, 8).map(_ toDouble)

// map list 2 into qtree
val buildQTree2 = list2.map { QTree(_) }.reduce { qtSemigroup.plus(_, _) }

// merge qtrees
val merged = buildQTree.merge(buildQTree2)

// keep list of qtree objects
val qTrees = List(buildQTree, buildQTree2)

// reduce qtrees together from qtree objects
val reduced = qTrees reduce(_.merge(_))

// filter qtrees
val sameRangeQTrees = qTrees filter(_.range == qTrees(0).range)

// filtered and reduced qtrees
val sameRangeReducedQTrees = qTrees filter(_.range == qTrees(0).range) reduce(_.merge(_))

// random generator for quantiles
val quantile = math.random

// returns low high of quantile
val (lower, upper) = buildQTree.quantileBounds(quantile)

val count = buildQTree.count
val rangeSumBounds = buildQTree.rangeSumBounds(lower, upper)
val rangeCountBounds = buildQTree.rangeCountBounds(lower, upper)
val compress = buildQTree.compress(1)
