package net.pushl.number {
  object Number {
    // 1,2,3 -> 1 | 4,5,6,7,8 -> 2 | 9... -> 3
    def sqrt_ceil(n : Long) : Long = {
      val EPS = 1e-8
      Math.sqrt(n.toDouble+EPS).toLong
    }
    final def doubling[T](x : T, n : Long, zero : T, bop : (T,T) => T) : T =
      if(n == 0) zero
      else if(n % 2 == 0) doubling(bop(x,x),n/2,zero,bop)
      else bop(doubling(bop(x,x),n/2,zero,bop),x)
  }
}
