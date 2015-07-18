import org.scalatest.FlatSpec


class UnionFindTreeTest extends FlatSpec {
  import net.pushl.datastructure.UnionFindTree

  "UnionFindTree test query_check" should "passed" in {
    val uf = UnionFindTree(9)
    val test_query = List(
        (0,1,2),
        (0,3,2),
        (1,1,3),
        (1,1,4),
        (0,2,4),
        (1,4,1),
        (0,4,2),
        (0,0,0),
        (1,0,0))

    val answer = test_query.map({case (q,a,b) => q match {
                                   case 0 => uf.unite(a,b); None
                                   case 1 => Some(uf.same(a,b))
                                   case _ => sys.error("no such query")
                                 }})
    assert(answer == List[Option[Boolean]](None,None,Some(true),Some(false),None,Some(true),None,None,Some(true)))
  }

}
