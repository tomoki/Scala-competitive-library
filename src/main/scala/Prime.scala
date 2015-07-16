package net.pushl.number {
  object Prime {
    // @return Array length is equal to 'n'.
    //  Kth element of Array is true if K is prime else false.
    final def sieve(n : Int) : Array[Boolean] = {
      val isprime = Array.fill(n+1)(true)
      isprime(0) = false
      isprime(1) = false
      for(i <- 2 to Math.ceil(Math.sqrt(n.toDouble)).toInt;
          if isprime(i);
          j <- i*i to n by i){
        isprime(j) = false
      }
      isprime
    }
    // @return Array of primes that is less than or equal to n
    final def primes(n : Int) : Array[Int] =
      sieve(n).zipWithIndex.withFilter(_._1).map(_._2)

    final def decomposition(n : Int, primes : Array[Int]) : Vector[Int] = {
      import scala.collection.immutable.VectorBuilder
      import scala.util.control.Breaks
      val v = new VectorBuilder[Int]()
      val b = new Breaks
      var c = n
      b.breakable {
        for(p <- primes){
          if(c == 1)
            b.break
          while(c % p == 0){
            c = c / p
            v += p
          }
        }
      }
      assert(c == 1, "after prime decomposition, remainder must be equal to 1")
      v.result
    }
    final def count_divisors(n : Long) : Int = {
      import Number.sqrt_ceil
      val u = sqrt_ceil(n)
      val l = if(u*u == n) -1 else 0
      val k = 2*(1l to u).count(n % _ == 0l)
      k + l
    }
  }
}
