import org.scalatest.FlatSpec
class TestRollingHash extends FlatSpec {
  import net.pushl.string.RollingHash
  val r = RollingHash((c : Char) => (c - 'a' + 1).toLong)
  "hash('abc').pushLeft('e')" should "hash('eabc')" in {
    assert(r.pushLeft(r("abc"),3,'k')  == r("kabc"))
  }
  "hash('abc').pushRight('e')" should "hash('eabc')" in {
    assert(r.pushRight(r("abc"),'e')  == r("abce"))
  }
  "hash('abc').popLeft('a')" should "hash('bc')" in {
    assert(r.popLeft(r("abc"),3,'a')  == r("bc"))
  }
  "hash('abc').popRight('c')" should "hash('ab')" in {
    assert(r.popRight(r("abc"),'c')  == r("ab"))
  }
  "hash('abcfsdf').popRight('f')" should "hash('abcfsdf')" in {
    assert(r.popRight(r("abcfsdf"),'f')  == r("abcfsd"))
  }
  "hash('abcfsdf').popRight('abcfsdf')" should "hash('abcfsdf')" in {
    assert(r.popRight(r("abcfsdf"),"abcfsdf")  == r("abcfsd"))
  }
  "hash('abcfsdf').popLeft('f')" should "hash('abcfsdf')" in {
    assert(r.popLeft(r("abcfsdf"),"abcfsdf")  == r("bcfsdf"))
  }
}
