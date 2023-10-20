object FlatPolyApp extends App {
  sealed case class Term(coeff: Int, expo: Int)

  class FlatPoly(val terms: List[Term]) { // val makes "terms" a member variable

    def unary_- : FlatPoly = {
      FlatPoly(terms.map {
        case Term(coeff, power) => Term(-coeff, power)
      })
    }

    def eval(x: Double): Double = {
      this.terms.foldLeft(0.0) {
        (acc, term) => acc + term.coeff * Math.pow(x, term.expo)
      }
    }

    def diff: FlatPoly = {
      FlatPoly(this.terms.map {
        case Term(coeff, power) if power > 0 => Term(coeff*power, power-1)
        case _ => Term(0, 0)
      }.filter(_.coeff!=0))
    }

    def normalize: FlatPoly = {
      val combine = this.terms.foldLeft(Map.empty[Int, Int]) {
        (acc, elem) => acc + (elem.expo -> (acc.getOrElse(elem.expo, 0)+elem.coeff))
      }
      val ret = combine.map { case (power, coeff) => Term(coeff, power) }
      FlatPoly(ret.toList.filter(_.coeff!=0).sortBy(_.expo))
    }

    def +(other: FlatPoly): FlatPoly = {
      FlatPoly(this.terms ++ other.terms)
    }

    // https://sjgpsoft.blogspot.com/2016/03/scala-collections-group-of-groupby.html <- I use this source
    def *(other: FlatPoly): FlatPoly = {
      // multiply all possible pairs
      val myProd = this.terms.flatMap {
        p => other.terms.map {
          q => Term(p.coeff*q.coeff, p.expo+q.expo)
        }
      }
      FlatPoly(myProd.groupBy(_.expo).map {
        case (power, myGroup) => Term(myGroup.map(_.coeff).reduce(_+_), power)
      }.toList.reverse)
    }

    override def toString: String =  {
      val renderedTerms = terms.map {
        case Term(1, 1) => "x"
        case Term(c, 0) => s"$c"
        case Term(c, 1) => s"${c}x"
        case Term(1, p) => s"x^$p"
        case Term(c, p) => s"${c}x^$p"
      }
      if terms.isEmpty then "<empty>" else renderedTerms.mkString(" + ")
    }
  }
  /*
    TEST CASES
  */
//   val a = FlatPoly(List(Term(2, 3), Term(3, 2), Term(5, 3)))
//   val b = FlatPoly(List(Term(1, 1), Term(1, 2), Term(2, 0)))
//   // eval
//   println(a.eval(1) == 10.0)
//   println(a.eval(2.5) == 128.125)
//   // diff
//   println(a.diff) // 6x^2 + 6x + 15x^2
//   println(b.diff) // 1 + 2x
//   // normalize
//   println(a.normalize) // 3x^2 + 7x^3
//   println(b.normalize) //  2 + x + x^2
//   // +
//   println(a+b) // 2x^3 + 3x^2 + 5x^3 + x + x^2 + 2
//   // *
//   println(a*b) // 10x^4 + 17x^3 + 6x^2 + 7x^5
}
