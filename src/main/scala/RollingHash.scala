package net.pushl.string {
  class RollingHash[T](val b : Long, val mod : Long, val conv : T => Long) {
    import net.pushl.number.Number.doubling
    final def hash(n : T) : Long = conv(n)%mod
    final def hash(n : Traversable[T]) = n.foldLeft(0l)((c,t) => (conv(t)+b*c)%mod)
    final def scan(n : Traversable[T]) = n.scanLeft(0l)((c,t) => (conv(t)+b*c)%mod)
    final def apply(n : T) = hash(n)
    final def apply(n : Traversable[T]) = hash(n)
    final def pushLeft(cur : Long, cur_length : Int, a : T) : Long =
      (conv(a)*b_pow_mod(cur_length.toLong) + cur) % mod
    final def pushRight(cur : Long, a : T) : Long =
      (b*cur + conv(a)) % mod
    final def popLeft(cur : Long, cur_length : Int, a : T) : Long =
      (cur - b_pow_mod(cur_length.toLong-1l)*conv(a) + mod) % mod
    final def popLeft(cur : Long, curt : Traversable[T]) : Long =
      this.popLeft(cur,curt.size,curt.head)
    final def popRight(cur : Long, a : T) : Long =
      (((cur - conv(a) + mod) % mod) * b_pow_mod(mod-2)) % mod
    final def popRight(cur : Long, curt : Traversable[T]) : Long =
      this.popRight(cur,curt.last)
    final def insert(scan_result : IndexedSeq[Long], what : T, i : Int) : Long = {
      val n = scan_result.length-1
      val left  = scan_result(i) * b_pow_mod((n-i).toLong) % mod
      val right = scan_result(n) - left + mod              % mod
      val ins   = hash(what) * b_pow_mod((n-i).toLong)     % mod
      (b * left + ins + right)  % mod
    }
    private[this] val pow_memo = scala.collection.mutable.HashMap[Long,Long]()
    final private def b_pow_mod(n : Long) = pow_memo.get(n) match {
        case Some(v) => v
        case None    => {
          pow_memo(n) = doubling(b,n,1l,(lhs : Long, rhs : Long) => lhs*rhs % mod)
          pow_memo(n)
        }
      }
  }
  object RollingHash {
    def apply[T](b : Long, mod : Long, conv : T => Long) = new RollingHash(b,mod,conv)
    def apply[T](conv : T => Long) = new RollingHash(19930319,1000000007,conv)
  }
}
