package net.pushl.datastructure {
  import scala.reflect.ClassTag
  import scala.annotation.tailrec
  abstract class ASegmentTree[T: ClassTag](_n: Int) {
    protected def init: T
    protected def calc(left: T, right: T): T
    private val n = smallest2Powered(_n)
    private val data = {
      val d = implicitly[ClassTag[T]].newArray(2*n-1)
      (0 until d.length).foreach(d(_) = init)
      d
    }
    def update(i: Int, v: T) : Unit = {
      val k = i+n-1
      data(k) = v
      @tailrec
      def updateAux(k: Int) : Unit =
        if(k > 0) {
          val p   = parent(k)
          data(p) = calc(data(getLeft(p)),data(getRight(p)))
          updateAux(p)
        }
      updateAux(k)
    }
    // get data of [a,b)
    def get(a: Int, b: Int) : T = get(a,b,0,0,n)

    // l,r is segment of node k.
    protected def get(a: Int, b: Int, k: Int, l: Int, r: Int) : T = {
      if(r <= a || b <= l) init
      else if(a <= l && r <= b) data(k)
      else calc(get(a,b,getLeft(k) ,l,(l+r)/2),
                get(a,b,getRight(k),(l+r)/2,r))
    }

    final private def getLeft (i: Int) = 2*i+1
    final private def getRight(i: Int) = 2*i+2
    final private def parent  (i: Int) = (i-1)/2
    final private def smallest2Powered(n: Int) = {
      @tailrec
      def smallest2PoweredAux(k: Int) : Int = {
        if(k >= n) k
        else smallest2PoweredAux(k*2)
      }
      smallest2PoweredAux(1)
    }
    override def toString() = data.foldLeft("[")(_ + "," + _) + "]"
  }
  class RangeSumQueryTree[T: ClassTag](n: Int)(implicit num: Numeric[T])
                                               extends ASegmentTree[T](n) {
    override def init = num.zero
    override def calc(left: T, right: T) = num.plus(left,right)
  }
  // Numeric does not have INF.
  class RangeMinimumQueryTree(n: Int) extends ASegmentTree[Int](n) {
    override def init = (1 << 31) - 1
    override def calc(left: Int, right: Int) = left min right
  }
}
