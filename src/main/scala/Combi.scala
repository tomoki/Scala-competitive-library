package net.pushl.number {
  object Combi {
    // choose r object from distinct n.
    def combi[T](n : T, r : T)(implicit num : Integral[T]) : T = {
      import num._
      if(n < r)
        zero
      else if(r > n-r)
        combi(n,n-r)
      else {
        var ans = one
        var i = zero
        while(i < r){
          ans = ans * (n-i)
          ans = ans / (i+one)
          i = i + one
        }
        ans
      }
    }
    // choose r object from n, but it's ok to choose same object
    // https://www.shinko-keirin.co.jp/keirinkan/kosu/mathematics/kirinuki/kirinuki58.html
    def multicombi[T](n : T, r : T)(implicit num : Integral[T]) : T = {
      import num._
      combi(n+r-one,r)
    }
  }
}
