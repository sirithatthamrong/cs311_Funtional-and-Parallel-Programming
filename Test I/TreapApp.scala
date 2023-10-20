object TreapApp extends App {
  sealed trait Treap[+K, +V]
  case object Empty extends Treap[Nothing, Nothing]
  case class Node[K, V](
                         left: Treap[K, V], key: K, value: V, right: Treap[K, V]
                       ) extends Treap[K, V]

  def getByValue[K, V](rt: Treap[K, V], v: V)(implicit ord: Ordering[K]): Option[(K, V)] = {
    case class FoundKey(loc: (K, V)) extends Exception
    def iterFind(node: Treap[K, V]): Option[(K, V)] = node match {
      case Empty => None
      case Node(left, key, value, right) =>
        iterFind(left)
        if (value==v) { throw FoundKey(key, value) }
        iterFind(right)
    }
    try { iterFind(rt) } catch {
      case FoundKey(loc) => Some(loc)
    }
  }

  def avgDepth[K, V](rt: Treap[K, V]): Double = {
    def findAvg(node: Treap[K, V], depth: Int): (Int, Int) = node match {
      case Empty => (0, 0)
      case Node(Empty, _, _, Empty) => (depth+1, 1)
      case Node(left, _, _, right) =>
        val (rd, rl) = findAvg(right, depth+1)
        val (ld, ll) = findAvg(left, depth+1)
        (ld+rd, ll+rl)
    }
    val (myDepth, myLeaves) = findAvg(rt, 0)
    if (myLeaves > 0) myDepth.toDouble/myLeaves
    else 0.0
  }

  def h[K](k: K): Int = {
    // h implements linear congruential compression on the hashCode
    // of the object. Both a and p are (decently large) prime numbers, and
    // b is arbitrarily chosen.
    val (a, b) = (879_190_747L, 32_452_843L)
    val p = 236_887_699L
    ((k.hashCode.toLong*a + b) % p).toInt
  }

  /*
   *       y                   x
   *      / \                 / \
   *     x   c    <=====>    a   y
   *    / \                     / \
   *   a   b                   b   c
  */

  def rebalance[K, V](rt: Treap[K, V]): Treap[K, V] = rt match {
    case Node(Node(a, xKey, xValue, b), yKey, yValue, c) if h(yKey) < h(xKey) =>
      Node(a, xKey, xValue, Node(b, yKey, yValue, c))
    case Node(a, xKey, xValue, Node(b, yKey, yValue, c)) if h(yKey) > h(xKey) =>
      Node(Node(a, xKey, xValue, b), yKey, yValue, c)
    case _ => rt
  }

  def insert[K, V](rt: Treap[K, V], k: K, v: V)(implicit ord: Ordering[K]): Treap[K, V] =
    rt match {
      case Empty => Node(Empty, k, v, Empty)
      case Node(l, key, value, r) => {
        val cmp = ord.compare(k, key)
        if cmp < 0 then rebalance(Node(insert(l, k, v), key, value, r))
        else if cmp == 0 then Node(l, key, v, r)
        else rebalance(Node(l, key, value, insert(r, k, v)))
      }
    }

  def satisfiesMHIV[K, V](rt: Treap[K, V]): Boolean = {
    def helper(each: TreapApp.Treap[K, V], mainKey: K) = each match {
      case Empty => true
      case Node(_, eachKey, _, _) =>
        h(mainKey) >= h(eachKey)
    }
    def each(rt: Treap[K, V]): Boolean = rt match {
      case Empty => true
      case Node(left, key, value, right) =>
        each(left) && each(right) && helper(left, key) && helper(right, key)
    }
    each(rt)
  }
  /*
    TEST CASES
  */
//  // getBtValue
//  val tree1 = Node(
//    Node(Empty, 1, "1", Node(Empty, 3, "3", Empty)),
//    5, "5",
//    Node(Empty, 10, "10", Node(Empty, 12, "12", Empty))
//  )
//  println(getByValue(tree1, "0").isEmpty)
//  println(getByValue(tree1, "12").contains((12, "12")))
//  println(getByValue(tree1, "1").contains((1, "1")))
//  // avgDepth
//  val emptyTree = Empty
//  val oneNode = Node(Empty, 1, "1", Empty)
//  val balanceTree = Node(Node(Empty, 2, "2", Empty), 1, "1", Node(Empty, 3, "3", Empty))
//  val unbalanceTree = Node(Empty, 1, "1", Node(Empty, 2, "2", Node(Empty, 3, "3", Empty)))
//  println(avgDepth(emptyTree) == 0.0)
//  println(avgDepth(oneNode) == 1.0)
//  println(avgDepth(balanceTree) == 2.0)
//  println(avgDepth(unbalanceTree) == 3.0)
//  // rebalanced
//  val tree2 = insert(insert(insert(Empty, 3, "3"), 1, "1"), 2, "2")
//  println(tree2) // Node(Empty,1,1,Node(Empty,2,2,Node(Empty,3,3,Empty)))
//  val tree3 = insert(Node(
//    Node(Empty, 1, "1", Node(Empty, 6, "6", Empty)),
//    11, "11",
//    Node(Empty, 10, "10", Empty)), 4, "4")
//  println(tree3) // Node(Node(Empty,1,1,Empty),4,4,Node(Node(Empty,6,6,Empty),11,11,Node(Empty,10,10,Empty)))
//  // satisfiesMHIV
//  val tree4 = Node(
//    Node(Empty, 8, "8", Node(Empty, 6, "6", Empty)),
//    11, "11",
//    Node(Empty, 12, "12", Node(Empty, 13, "13", Empty))
//  )
//  println(rebalance(tree4))
//  println(satisfiesMHIV(tree4) == true)
//  val tree5 = Node(
//    Node(Empty, 5, "5", Node(Empty, 3, "3", Empty)),
//    10, "10",
//    Node(Empty, 8, "8", Node(Empty, 7, "7", Empty))
//  )
//  val res1 = rebalance(tree5) // Node(Empty,5,5,Node(Node(Empty,3,3,Empty),10,10,Node(Empty,8,8,Node(Empty,7,7,Empty))))
//  println(satisfiesMHIV(tree5))
}
