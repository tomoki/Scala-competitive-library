package net.pushl.number {
  object Optimize {
    // find lowest value that satisfy f
    //  lower should NOT satisfy f
    //  upper should satisfy f
    def binary_search(f : (Double) => Boolean,
                      lower : Double, upper : Double) : Double = {
      assert(!f(lower), s"f(lower) should be false (lower = $lower)")
      assert(f(upper)    , s"f(upper) should be true (upper  = $lower)")
      var l = lower
      var u = upper
      for(i <- 0 to 200) {
        val m = (l+u)/2
        if(f(m)) u = m
        else l = m
      }
      u
    }
    // 凸関数の極大な点をもとめる
    def ternary_search(f     : (Double) => Double,
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
    def ternary_search_concave(f     : Double => Double,
                               left  : Double,
                               right : Double) : Double = {
      ternary_search(-f(_),left,right)
    }
  }
}
