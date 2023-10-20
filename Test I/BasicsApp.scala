object BasicsApp extends App {
  def flatMap[A, B](f: A => List[B])(xs: List[A]): List[B] = {
    def eachOf(xs: List[A], ans: List[B]): List[B] = xs match {
      case Nil => ans.reverse
      case h::t => eachOf(t, f(h).foldLeft(ans)((acc, elem) => elem::acc))
    }
    eachOf(xs, Nil)
  }

  def negNegPos(n: Int): List[Int] = {
    val num: List[Int] = (1 to n).toList
    flatMap((x: Int) => List(-x, -x, x))(num)
  }

  def winnerOf(xs: Vector[String]): (String, Int) = {
    val freq = xs.foldLeft(Map.empty[String, Int]) {
      (acc, name) => acc + (name -> (acc.getOrElse(name, 0)+1)) }
    freq.maxBy(_._2)
  }

  def nonTrailingZeros(n: BigInt): Int = {
    val digits = n.toString.foldLeft(0, 0) {
      case ((count, zero), '0') => (count, zero+1)
      case ((count, zero), _) => (count+zero, 0)
    }
    digits._1
  }
  /*
    TEST CASES
  */
//  // flatMap
//  val foo = (x: Int) => if x < 0 then Nil else List(1, x, x * x)
//  val xs = List(3, -4, -5, 2)
//  val ys = flatMap(foo)(xs)
//  println(ys == List(1, 3, 9, 1, 2, 4))
//  val bar = (x: String) => List(x.length / 2.0, 0.5 + x.length)
//  val zs = List("hi", "hello")
//  val ts = flatMap(bar)(zs)
//  println(ts == List(1.0, 2.5, 2.5, 5.5))
//  // negNegPos
//  println(negNegPos(1) == List(-1, -1, 1))
//  println(negNegPos(2) == List(-1, -1, 1, -2, -2, 2))
//  println(negNegPos(3) == List(-1, -1, 1, -2, -2, 2, -3, -3, 3))
//  // winnerOf
//  println(winnerOf(Vector("Dan", "Alice", "Bob", "Bob", "Alice", "Bob", "Dylan")) == ("Bob", 3))
//  println(winnerOf(Vector("ling", "ling", "ling", "eng")) == ("ling", 3))
//  // nonTrailingZeros
//  println(nonTrailingZeros(20300) == 1)
//  println(nonTrailingZeros(4040005010L) == 5)
//  println(nonTrailingZeros(123400000) == 0)
//  println(nonTrailingZeros(BigInt("1000010000101000000000")) == 9)
}