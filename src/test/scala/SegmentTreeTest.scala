import org.scalatest.FlatSpec
class SegmentTreeTest extends FlatSpec {
  import net.pushl.datastructure.{RangeSumQueryTree, RangeMinimumQueryTree}
  ("RangeSumQuery (DSL_2_B test1 http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=1452709#3)"
     should "passed") in {
    val rsq = new RangeSumQueryTree[Int](5)
    // com x y
    // if com = 0 => Ax += y
    // if com = 1 => sum of Ax ~ Ay
    val test_query = Vector((0, 1, 3),
                            (0, 2, 1),
                            (0, 3, 4),
                            (1, 0, 1),
                            (1, 0, 2),
                            (1, 2, 2),
                            (1, 0, 4))

    val ans = Vector(-1,-1,-1,3,4,1,8)
    assert(ans == test_query.map({case (com,x,y) => {
                      if(com == 0){
                        rsq(x) = rsq.get(x,x+1) + y
                        -1
                      }else{
                        rsq.get(x,y+1)
                      }
                    }}))
  }
}
