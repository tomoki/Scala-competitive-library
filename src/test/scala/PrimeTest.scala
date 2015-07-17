import org.scalatest.FlatSpec

class TestPrimes extends FlatSpec{
  import net.pushl.number.Prime
  val n = 10
  val isp = Prime.sieve(40)
  "1" should "not prime" in { assert(isp(1) == false) }
  "2" should "prime"     in { assert(isp(2) == true)  }
  "3" should "prime"     in { assert(isp(3) == true)  }
  "4" should "not prime" in { assert(isp(4) == false) }
  "5" should "prime"     in { assert(isp(5) == true)  }
  "6" should "not prime" in { assert(isp(6) == false) }
  "7" should "prime"     in { assert(isp(7) == true)  }
  "8" should "not prime" in { assert(isp(8) == false) }
  "9" should "not prie" in { assert(isp(9) == false) }

  "prime_decompsition(24,primes)" should "[2,2,2,3]" in {
    assert(Prime.decomposition(24,Prime.primes(25)) == Vector(2,2,2,3))
  }
}
