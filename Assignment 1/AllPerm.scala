object AllPerm extends App {
  def allPerm(n: Int): List[List[Int]] = {

    def insert(n: Int, numAdd: Int, count: Int, list: List[Int], backup: List[Int]): List[Int] = {
      if (count == n)
        backup ::: numAdd :: list
      else
        insert(n, numAdd, count + 1, list.tail, list.head :: backup)
    }

    /*
      Take each list from LIST in addAll -> insert numAdd
      list = (1,2)
      count = numAdd = 3
      RETURN: [(3,1,2), (1,3,2), (1,2,3)]
    */
    def addEach(count: Int, numAdd: Int, list: List[Int], ans: List[List[Int]]): List[List[Int]] = {
      def addEachHelper(count: Int, numAdd: Int, list: List[Int], ans: List[List[Int]], acc: List[List[Int]]): List[List[Int]] = {
        if (count == 0) ans.reverse ::: acc
        else {
          val updated = insert(count - 1, numAdd, 0, list, Nil)
          addEachHelper(count - 1, numAdd, list, ans, updated :: acc)
        }
      }
      addEachHelper(count, numAdd, list, ans, Nil)
    }

    /*
      list = [(1,2), (2,1)]
      numAdd = 3
      RETURN: [(3,1,2), (1,3,2), (1,2,3), (3,2,1), (2,3,1), (2,1,3)]
    */
    def addAll(list: List[List[Int]], size: Int, numAdd: Int): List[List[Int]] = {
      def addAllHelper(list: List[List[Int]], size: Int, numAdd: Int, acc: List[List[Int]]): List[List[Int]] = size match {
        case 0 => acc
        case _ =>
          val added = addEach(numAdd, numAdd, list.head, Nil)
          addAllHelper(list.tail, size - 1, numAdd, added ::: acc)
      }
      addAllHelper(list, size, numAdd, Nil)
    }

    def prep(n: Int): List[List[Int]] = n match {
      case 1 => List(List(1))
      case _ =>
        val prev = prep(n - 1)
        addAll(prev, prev.length, n)
    }
    prep(n)
  }

//  val startTime = System.nanoTime()
//  println(allPerm(2))
//  println(allPerm(3))
//  println(allPerm(4))
//  println(allPerm(10))
//  val endTime = System.nanoTime()
//  val elapsedTime = endTime - startTime
//  println(s"Elapsed time: ${elapsedTime / 1e6} ms")
}
