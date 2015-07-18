import org.scalatest.FlatSpec

class PointTest extends FlatSpec {
  import net.pushl.geometry.Point

  val EPS = 1e-8
  val a = Point(1.0,2.0)
  val b = Point(1.0,3.0)
  val c = Point(1.0+EPS,2.0)
  val d = Point(1.0,2.0+EPS)
  val e = Point(2.0,2.0)
  val k = Point(3.0,4.0)
  val l = Point(-2.0,-2.0)


  s"$a compare $b" should "-1" in {
    assert((a compare b) == -1)
  }
  s"$a compare $e" should "-1" in {
    assert((a compare e) == -1)
  }
  s"$b compare $a" should "1" in {
    assert((b compare a) == +1)
  }
  s"$e compare $a" should "1" in {
    assert((e compare a) == +1)
  }
  s"$a compare $c" should "0" in {
    assert((a compare c) == 0)
  }
  s"$c compare $a" should "0" in {
    assert((c compare a) == 0)
  }
  s"$a compare $d" should "0" in {
    assert((a compare d) == 0)
  }
  s"$d compare $a" should "0" in {
    assert((d compare a) == 0)
  }
  s"$a == $a" should "true" in {
    assert(a == a)
  }
  s"$a == $c" should "true" in {
    assert(a == c)
  }
  s"$c == $a" should "true" in {
    assert(c == a)
  }
  s"$c == $c" should "true" in {
    assert(c == c)
  }
  s"$a + $e" should s"$k" in {
    assert(a + e == k)
  }
  s"$a - $k" should s"$l" in {
    assert(a - k == l)
  }
  s"$a.hashCode == $c.hashCode" should "true" in {
    assert(a.hashCode == c.hashCode)
  }
  s"$a.hashCode == $d.hashCode" should "true" in {
    assert(a.hashCode == d.hashCode)
  }
  val h1 = Point(1.5+EPS,2.5)
  val h2 = Point(1.5,2.5-EPS)
  val h3 = Point(1.5-EPS,2.0)
  val h4 = Point(1.5,2.5+EPS)
  val h5 = Point(1.5,2.5)

  s"$h5.hashCode == $h1.hashCode" should "true" in {
    assert(h5.hashCode == h1.hashCode)
  }
  s"$h5.hashCode == $h2.hashCode" should "true" in {
    assert(h5.hashCode == h2.hashCode)
  }
  s"$h5.hashCode == $h3.hashCode" should "true" in {
    assert(h5.hashCode == h3.hashCode)
  }
  s"$h5.hashCode == $h4.hashCode" should "true" in {
    assert(h5.hashCode == h4.hashCode)
  }
  s"$h5.hashCode == $h5.hashCode" should "true" in {
    assert(h5.hashCode == h5.hashCode)
  }
}
