package net.pushl.number {
  object Optimize {
    // find lowest value that satisfy f
    //  lower should NOT satisfy f
    //  upper should satisfy f
    def binarySearch(f : (Double) => Boolean,
                      lower : Double, upper : Double) : Double = {
      require(f(lower) == false,  s"f(lower) should be false (lower = $lower)")
      require(f(upper) == true,   s"f(upper) should be true (upper = $upper)")
      var l = lower
      var u = upper
      for(i <- 0 to 200) {
        val m = (l+u)/2
        if(f(m)) u = m
        else l = m
      }
      u
    }
    // find largest value that satisfy f
    //  lower should satisfy f
    //  upper should NOT satisfy f
    // [true, true, true, false, false, false....]
    def binarySearch2(f : (Double) => Boolean,
                      lower : Double, upper : Double) : Double = {
      require(f(lower) == true,  s"f(lower) should be true (lower = $lower)")
      require(f(upper) == false, s"f(upper) should be false (upper = $upper)")
      var l = lower
      var u = upper
      for(i <- 0 to 200) {
        val m = (l+u)/2
        if(f(m)) l = m
        else u = m
      }
      l
    }

    // 凸関数の極大な点をもとめる
    def ternarySearch(f     : (Double) => Double,
                       left  : Double,
                       right : Double) : Double = {
      var l = left
      var r = right
      for(i <- 0 to 200) {
        val ls = (2*l +   r) / 3
        val rs = (l   + 2*r) / 3
        if(f(ls) < f(r))
          l = ls
        else
          r = rs
      }
      (l+r)/2
    }
    // 凹関数の極小な点を求める
    def ternarySearchConcave(f     : Double => Double,
                               left  : Double,
                               right : Double) : Double = {
      ternarySearch(-f(_),left,right)
    }
  }
}
