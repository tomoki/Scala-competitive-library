import org.scalatest.FlatSpec

class LineTest extends FlatSpec {
  import net.pushl.geometry._
  val segments = Vector(Segment(Point(0,0),Point(0,1)),
                        Segment(Point(0,2),Point(0,3)),
                        Segment(Point(1,0),Point(1,1)),
                        Segment(Point(1,2),Point(1,3)))

  val s0 = segments(0)
  val s1 = segments(1)
  val s2 = segments(2)
  val s3 = segments(3)
  val line = Line(Point(1,1),Point(0,1))

  s"$line and $s0" should "intersected" in {
    assert(line.isIntersectedTo(segments(0)))
  }
  s"$line and $s1" should "not intersected" in {
    assert(!(line.isIntersectedTo(segments(1))))
  }
  s"$line and $s2" should "intersected" in {
    assert(line.isIntersectedTo(segments(2)))
  }
  s"$line and $s3" should "not intersected" in {
    assert(!line.isIntersectedTo(segments(3)))
  }

  def go(segments : Vector[Segment]) =
    println(segments.flatMap((s1) =>
      segments.flatMap((s2) => {
                         val cand = List(s1.a,s1.b,s2.a,s2.b)
                         cand.flatMap((c1) =>
                           cand.map((c2) =>
                             if(c1 == c2) 0
                             else segments.count(Line(c1,c2).isIntersectedTo(_))))
                       })))
  val segments2 = Vector(Segment(Point(0,0),Point(0,1)),
                         Segment(Point(0,1),Point(0,2)),
                         Segment(Point(1,0),Point(1,1)),
                         Segment(Point(1,1),Point(1,2)))

  val s02 = segments2(0)
  val s12 = segments2(1)
  val s22 = segments2(2)
  val s32 = segments2(3)


  val good = Line(Point(0,1),Point(1,1))
  s"$good and $s02" should "intersected" in {
    assert(good.isIntersectedTo(s02))
  }
  s"$good and $s12" should "intersected" in {
    assert(good.isIntersectedTo(s12))
  }
  s"$good and $s22" should "intersected" in {
    assert(good.isIntersectedTo(s22))
  }
  s"$good and $s32" should "intersected" in {
    assert(good.isIntersectedTo(s32))
  }
  val pb = s02.b
  s"$good should contain $pb" should "true" in {
    assert(good.onThisLine(pb))
  }

  s"$good and $pb are very near" should "true" in {
    assert(good.distanceTo(pb) < 0.001)
  }
}
