object DateUtil extends App {
  type Date = (Int, Int, Int)

  def isOlder(x: Date, y: Date): Boolean = {
    if (x._3 < y._3) true
    else if (x._3 == y._3 && x._2 < y._2) true
    else if (x._3 == y._3 && x._2 == y._2 && x._1 < y._1) true
    else false
  }
//  println(isOlder((1,2,2000), (5,6,2005)) == true)
//  println(isOlder((1,2,2005), (5,6,2005)) == true)
//  println(isOlder((1,7,2005), (5,6,2005)) == false)
//  println(isOlder((5,6,2005), (5,6,2005)) == false)


  def numberInMonth(xs: List[Date], month: Int): Int = {
    xs.count((d,m,y) => m == month)
  }
//  val dates = List((1,2,2001), (3,7,1998), (15,7,2004), (5,6,2005))
//  println(numberInMonth(dates,7) == 2)
//  println(numberInMonth(dates,8) == 0)
//  println(numberInMonth(dates,6) == 1)

  def numberInMonths(xs: List[Date], months: List[Int]): Int = {
    xs.count((d,m,y) => months.contains(m))
  }
//  val dates = List((1,2,2022), (15,3,2022), (8,4,2022), (20,2,2022), (5,5,2022))
//  println(numberInMonths(dates, List(2,3)) == 3)
//  println(numberInMonths(dates, List(2)) == 2)
//  println(numberInMonths(dates, List(4,5)) == 2)

  def datesInMonth(xs: List[Date], month: Int): List[Date] = {
    xs.filter((d,m,y) => m == month)
  }
//  val dates = List((18,2,2002), (15,1,2021), (8,3,2022), (20,2,2019),
//                    (5,5,2022), (16,5,2001), (5,6,2005), (17,5,2007))
//  println(datesInMonth(dates, 2) == List((18,2,2002), (20,2,2019)))
//  println(datesInMonth(dates, 5) == List((5,5,2022), (16,5,2001), (17,5,2007)))

  def datesInMonths(xs: List[Date], months: List[Int]): List[Date] = {
    xs.filter((d,m,y) => months.contains(m))
  }
//    val dates = List((18,2,2002), (15,1,2021), (8,3,2022), (20,2,2019),
//                      (5,5,2022), (16,5,2001), (5,6,2005), (17,5,2007))
//    println(datesInMonths(dates, List(1,2)) == List((18,2,2002), (15,1,2021), (20,2,2019)))
//    println(datesInMonths(dates, List(5,6)) == List((5,5,2022), (16,5,2001), (5,6,2005), (17,5,2007)))

  def dateToString(d: Date): String = {
    val months = Map(
      1->"January", 2->"February", 3->"March", 4->"April", 5->"May", 6->"June",
      7->"July", 8->"August", 9->"September", 10->"October", 11->"November", 12->"December")
    months(d._2) + "-" + d._1.toString + "-" + d._3.toString
  }
//  println(dateToString((5,6,2005)) == "June-5-2005")
//  println(dateToString((1,12,2000)) == "December-1-2000")

  def whatMonth(n: Int, yr: Int): Int = {
    def findMonth(day: Int, dm: List[Int]): Int = {
      if (day-dm.head <= 0) 1
      else 1 + findMonth(day-dm.head, dm.tail)
    }
    if ((yr % 4 == 0 && yr % 100 != 0) || (yr % 400 == 0))
      findMonth(n, List(31,29,31,30,31,30,31,31,30,31,30,31))
    else findMonth(n, List(31,28,31,30,31,30,31,31,30,31,30,31))
  }
//  println(whatMonth(1,2023) == 1)
//  println(whatMonth(366,2024) == 12)
//  println(whatMonth(60,2022) == 3)
//  println(whatMonth(125,2023) == 5)


  def oldest(dates: List[Date]): Option[Date] = {
    if (dates.isEmpty) None
    else Some(dates.minBy((d,m,y) => (y,m,d)))
  }
//  println(oldest(List((5,10,2020), (15,8,2019), (1,1,2021))) == Some(15,8,2019))
//  println(oldest(List((5,10,1999), (15,8,2019), (1,1,2021))) == Some(5,10,1999))

  def isReasonableDate(d: Date): Boolean = {
    def find(dm: List[Int]): Boolean = d._1 <= dm(d._2-1)
    if (d._3<=0 || d._2<1 || d._2>12 || d._1<1 || d._1>31) false
    else if (d._3 % 4 == 0 && d._3 % 100 != 0 || (d._3 % 400 == 0))
      find(List(31,29,31,30,31,30,31,31,30,31,30,31))
    else find(List(31,28,31,30,31,30,31,31,30,31,30,31))
  }
//  println(isReasonableDate(29,2,1998) == false)
//  println(isReasonableDate(29,2,1999) == false)
//  println(isReasonableDate(29,2,2000) == true)
//  println(isReasonableDate(35,4,2000) == false)
}
