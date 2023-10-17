object GraphBFS extends App {
  /*
    nbrs: a func that provides neighbors for a given node
    src: where the BFS should start
  */
  def bfs[V](nbrs: V => Set[V], src: V) = {
    def expand(frontier: Set[V], parent: Map[V, V]): (Set[V], Map[V, V]) = {
      /*
        newFrontier: store nodes to be explored in the next iteration
        newParent: a map that records the parent of each node
        queue: a set of nodes to be visited in the current iteration
      */
      def expandHelper(newFrontier: Set[V], newParent: Map[V, V], queue: Set[V]): (Set[V], Map[V, V]) = queue.size match {
        case 0 => (newFrontier, newParent)
        case _ =>
            // a set of new frontier, filter out nbrs that already in parent (avoid revisited)
            val tempFrontier: Set[V] = nbrs(queue.head).filterNot(parent.contains)
            expandHelper(
              newFrontier ++ tempFrontier,
              // append a map (child -> parent), all child have queue.head as a parent
              newParent ++ tempFrontier.foldLeft(Map.empty[V, V])((acc, v) => acc + (v -> queue.head)), queue.tail)
      }
      expandHelper(Set.empty, parent, frontier)
    }

    def iterate(frontier: Set[V], parent: Map[V, V], distance: Map[V, Int], d: Int): (Map[V, V], Map[V, Int]) = {
      if (frontier.isEmpty) (parent, distance)
      else {
        val (myFrontier, myParent) = expand(frontier, parent)
        val distance_ = distance ++ frontier.foldLeft(Map.empty[V, Int])((acc, v) => acc +(v -> d))
        iterate(myFrontier, myParent, distance_, d + 1)
      }
    }
    iterate(Set(src), Map(src -> src), Map(), 0)
  }
//  def nbrs(node: String): Set[String] = {
//    if (node.equals("A")) Set("B", "C", "G")
//    else if (node.equals("B")) Set("A", "C", "D")
//    else if (node.equals("C")) Set("A", "B", "F")
//    else if (node.equals("D")) Set("B", "E")
//    else if (node.equals("E")) Set("D", "F", "G")
//    else if (node.equals("F")) Set("C", "E", "G")
//    else if (node.equals("G")) Set("A", "F")
//    else Set()
//  }
//  println(bfs(nbrs, "A"))
}

