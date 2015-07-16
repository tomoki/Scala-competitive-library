package net.pushl.number {
  object Number {
    // 1,2,3 -> 1 | 4,5,6,7,8 -> 2 | 9... -> 3
    def sqrt_ceil(n : Long) : Long = {
      val EPS = 1e-8
      Math.sqrt(n.toDouble+EPS).toLong
    }
    // TODO write test
    final def doubling[T](x : T, n : Long, zero : T, bop : (T,T) => T) : T =
      if(n == 0) zero
      else if(n % 2 == 0) doubling(bop(x,x),n/2,zero,bop)
      else bop(doubling(bop(x,x),n/2,zero,bop),x)

    // TODO: Write test
    def gcd(a : Long, b : Long) : Long = {
      if(b == 0) a
      else gcd(b,a%b)
    }
    // TODO: Write test
    def lcm(a : Long, b : Long) : Long = {
      if(a < 0) lcm(-a,b)
      else if(b < 0) lcm(a,-b)
      else a*b / gcd(a,b)
    }
  }
}
