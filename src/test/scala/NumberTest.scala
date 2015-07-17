import org.scalatest.FlatSpec

class TestNumber extends FlatSpec{
  import net.pushl.number.Number._

  "sqrt_ceil(1)" should "1" in {
    assert(sqrt_ceil(1) == 1)
  }
  "sqrt_ceil(2)" should "1" in {
    assert(sqrt_ceil(2) == 1)
  }
  "sqrt_ceil(3)" should "1" in {
    assert(sqrt_ceil(3) == 1)
  }
  "sqrt_ceil(4)" should "2" in {
    assert(sqrt_ceil(4) == 2)
  }
  "sqrt_ceil(9)" should "3" in {
    assert(sqrt_ceil(9) == 3)
  }
  "sqrt_ceil(11)" should "3" in {
    assert(sqrt_ceil(11) == 3)
  }
}
