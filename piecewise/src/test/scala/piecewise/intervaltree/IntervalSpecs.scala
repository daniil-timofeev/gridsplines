package piecewise
package intervaltree
import cats.instances.double._
import org.specs2._

class IntervalSpecs extends Specification {def is = s2"""
  Correct creation of non empty interval $nonEmptyInterval
  Correct creation of empty interval $emptyInterval
  Correct behaviour of left closed right open interval $leftClosedRightOpenInterval
  Correct behaviour of closed interval $closedInterval
  Correct behaviour of open interval $openInterval
  Correct behaviour of left open right closed interval $leftOpenRightClosedInterval
  Correct separation into segments $leftOpenRightClosedSegments
  Correct separation into segments by width $leftOpenRightClosedSegmentsByWidth
  """

  def nonEmptyInterval =
    CsdOpn.pure.apply(20.0, 30.0).isEmpty must beFalse


  def emptyInterval =
    Csd.pure.apply(30.0, 20.0).isEmpty must beTrue


  def leftClosedRightOpenInterval = {

    val i = CsdOpn.pure.unsafe(20.0, 30.0)
    val i0 = CsdOpn.pure.unsafe(1.0, 1.0)

    val trueMathers =
      Seq(Interval.contains(i)(20.0)
        , Interval.contains(i)(25.0)
      ) must contain(beTrue).forall

    val falseMathers =
      Seq(Interval.contains(i)(30.0)
        , Interval.contains(i)(10.0)
        , Interval.contains(i)(35.0)
        ,  Interval.contains(i0)(1.0)
      ) must contain(beFalse).forall

    trueMathers and falseMathers
  }

  def closedInterval = {

    val i = Csd.pure.unsafe(20.0, 30.0)
    val i0 = Csd.pure.unsafe(1.0, 1.0)

    val trueMathers =
      Seq(Interval.contains(i)(20.0)
        , Interval.contains(i)(25.0)
        , Interval.contains(i)(30.0)
        , Interval.contains(i0)(1.0)
      ) must contain(beTrue).forall

    val falseMathers =
      Seq(Interval.contains(i)(10.0)
        , Interval.contains(i)(35.0)
      ) must contain(beFalse).forall

    trueMathers and falseMathers
  }

  def openInterval = {

    val i = Opn.pure.unsafe(20.0, 30.0)
    val i0 = Opn.pure.unsafe(1.0, 1.0)

    val trueMathers =
      Seq(Interval.contains(i)(25.0)) must contain(beTrue).forall

    val falseMathers =
      Seq(Interval.contains(i)(20.0)
        , Interval.contains(i)(30.0)
        , Interval.contains(i)(10.0)
        , Interval.contains(i)(35.0)
        , Interval.contains(i0)(1.0)
      ) must contain(beFalse).forall

    trueMathers and falseMathers
  }

  def leftOpenRightClosedInterval = {

    val i = OpnCsd.pure.unsafe(20.0, 30.0)
    val i0 = OpnCsd.pure.unsafe(1.0, 1.0)

    val trueMathers =
      Seq(Interval.contains(i)(25.0)
        , Interval.contains(i)(30.0)
      ) must contain(beTrue).forall

    val falseMathers =
      Seq(Interval.contains(i)(20.0)
        , Interval.contains(i)(10.0)
        , Interval.contains(i)(35.0)
        , Interval.contains(i0)(1.0)
      ) must contain(beFalse).forall

    trueMathers and falseMathers
  }

  def leftOpenRightClosedSegments = {
    val i = OpnCsd.pure.unsafe(20.0, 30.0)
    import algebra.instances.all._

    val sect = OpnCsd.pure.sections(10)(i)

    {sect must haveSize(10)} and {sect.last.upper.bound must_==(30.0)}
  }

  def leftOpenRightClosedSegmentsByWidth = {
    val i = OpnCsd.pure.unsafe(20.0, 30.0)
    import algebra.instances.all._

    val sect = OpnCsd.pure.sectionsWithMaxLength(1.2)(i)

    {sect must haveSize(9) and {sect.last.upper.bound must_==(30.0)}}
  }

}
