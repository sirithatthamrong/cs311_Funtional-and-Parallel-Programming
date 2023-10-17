object Aggregate extends App{
  def myMin(p: Double, q: Double, r: Double): Double = {
    def compare(x: Double, y: Double) = { if (x <= y) x else y }
    compare(compare(p,q), compare(q,r))
  }
//  println(myMin(3.0, 1.0, 9.0)) // 1.0
//  println(myMin(6.0, -6, -6.000001)) // -6.000001
//  println(myMin(1.1, 1.12, 1.13)) // 1.1

  def myMean(p: Double, q: Double, r: Double): Double = { (p+q+r)/3 }
//  println(myMean(3, 7, 4)) // 4.666666666666667
//  println(myMean(2, 100, 15)) // 39
//  println(myMean(7, 15, -6)) // 5.333333333333333

  def myMed(p: Double, q: Double, r: Double): Double = {
    if ((p <= q && q <= r) || (r <= q && q <= p)) q
    else if ((q <= p && p <= r) || (r <= p && p <= q)) p
    else r
  }
//  println(myMed(13, 5.0, 12)) // 12.0
//  println(myMed(4, 1, 5)) // 4.0
//  println(myMed(0.1, -0.5, 3)) // 0.1
//  println(myMed(-3.1, -0.5, 3)) // -0.5
//  println(myMed(-0.1, 0.5, 3)) // 0.5
//  println(myMed(2, 2, 2)) // 2.0
//  println(myMed(2.13, 2.2, 0.3)) // 2.13
//  println(myMed(2.2, 2.13, 0.3)) // 2.13
//  println(myMed(2.2, 0.3, 2.13)) // 2.13
}

