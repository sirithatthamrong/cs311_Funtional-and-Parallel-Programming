object StreamApp extends App {
  def onlyOddOdds(s: LazyList[Long]): LazyList[Long] = {
    s.filter(_.toString.count(y => y=='1'||y=='3'||y =='5'||y =='7'||y=='9') % 2 != 0)
  }

  val allOddOdds: LazyList[Long] = {
    LazyList.from(1).map(_.toLong)
      .filter(_.toString.count(y => y=='1'||y=='3'||y =='5'||y =='7'||y=='9') % 2 != 0)
  }

  def recurrence(r: Vector[(Int, Int)]): LazyList[Long] = {
    val (coeffs, baseCases) = r.unzip
    def next(s: LazyList[Long]): LazyList[Long] = {
      val myNext = coeffs.zip(s.reverse).foldLeft(0L) {
        case (acc, (coeff, base)) => acc + (coeff*base)
      }
      myNext #:: next(s.tail :+ myNext)
    }
    LazyList.from(baseCases.map(_.toLong)) #::: next(LazyList.from(baseCases.map(_.toLong)))
  }

  val fibOddOdds: LazyList[Long] = {
    recurrence(Vector((1, 1), (1, 1)))
      .filter(_.toString.count(y => y=='1'||y=='3'||y=='5'||y=='7'||y=='9') % 2 != 0)
  }
  /*
    TEST CASES
  */
//  // onlyOddOdds
//  println(onlyOddOdds(LazyList.range(1, 150)).toList == List(1, 3, 5, 7, 9, 10, 12, 14, 16, 18, 21, 23, 25, 27, 29, 30,
//    32, 34, 36, 38, 41, 43, 45, 47, 49, 50, 52, 54, 56, 58, 61, 63, 65, 67, 69, 70, 72, 74, 76, 78, 81, 83, 85, 87, 89,
//    90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 111, 113, 115, 117, 119, 120, 122, 124, 126, 128, 131, 133, 135, 137,
//    139, 140, 142, 144, 146, 148)) // length = 75
//  // allOddOdds
//  println(allOddOdds.take(20).toList == List(1, 3, 5, 7, 9, 10, 12, 14, 16, 18, 21, 23, 25, 27, 29, 30, 32, 34, 36, 38))
//  // recurrence
//  val coeff1 = Vector((1, 1), (1, 1))
//  println(recurrence(coeff1).take(5).toList == List(1, 1, 2, 3, 5))
//  val coeff2 = Vector((2, 2), (3, 1))
//  println(recurrence(coeff2).take(5).toList == List(2, 1, 8, 19, 62))
//  // fibOddOdds
//  println(fibOddOdds.take(20).toList == List(1, 1, 3, 5, 21, 34, 89, 144, 377, 610, 2584, 17711, 46368, 75025, 121393,
//    196418, 317811, 514229, 832040, 1346269))
}