object Aloud extends App {
  def readAloud(xs: List[Int]): List[Int] = {
    def sum(count: Int, myList: List[Int], ans: List[Int]): List[Int] = myList match {
      case Nil => ans.reverse
      case h :: t =>
        if (t.isEmpty) sum(0, Nil, h :: count+1 :: ans)
        else if (h == t.head) sum(count+1, t, ans)
        else sum(0, t, h :: count+1 :: ans)
    }
    sum(0, xs, Nil)
  }
//  println(readAloud(List()) == Nil)
//  println(readAloud(List(1,1,1)) == List(3, 1))
//  println(readAloud(List(1,1,2)) == List(2,1,1,2))
//  println(readAloud(List(-1,2,7)) == List(1,-1,1,2,1,7))
//  println(readAloud(List(3,3,8,-10,-10,-10)) == List(2,3,1,8,3,-10))
//  println(readAloud(List(3,3,1,1,3,1,1)) == List(2,3,2,1,1,3,2,1))
}
