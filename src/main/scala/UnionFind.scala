import scala.annotation.tailrec
package net.pushl.datastructure {
  class UnionFindTree(val n: Int) {
    // if data[i] is less than 0, i is root and -data[i] is the size of tree
    // else, data[i] is parent of i
    private[this] val data = Array.fill(n)(-1)
    private def isRoot(k: Int) = data(k) < 0
    @tailrec
    private def getRoot(k: Int, log: List[Int] = List[Int]()) : Int =
      if(isRoot(k)) { log.foreach(data(_) = k); k }
      else getRoot(data(k),k :: log)
    def getGroupSize(k: Int)  : Int     = -data(getRoot(k))
    def same (a: Int, b: Int) : Boolean = getRoot(a) == getRoot(b)
    def unite(a: Int, b: Int) : Boolean = {
      val x = getRoot(a)
      val y = getRoot(b)
      if(x != y){
        // connect lower to higher
        val (f,t) = if(getGroupSize(x) > getGroupSize(y))  (x,y)
                    else                                   (y,x)
        data(t) += data(f)
        data(f) = t
        true
      }else
        false
    }
  }
  object UnionFindTree {
    def apply(n: Int) = if(n < 0) sys.error("Size of unionfind should be greater than or equal to 0")
                        else new UnionFindTree(n)
  }
}
