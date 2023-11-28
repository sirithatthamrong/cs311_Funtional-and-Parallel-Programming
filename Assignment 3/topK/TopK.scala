import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object TopK {

  def wordCriteria(word: String): Boolean = {
    // ^[a-z] = start with a lowercase letter [a-z]
    // [a-z-]* = follow by zero or more lowercase letters or -
    // [a-z]$ = end with a lowercase letter [a-z]
    val pattern = "^[a-z][a-z-]*[a-z]$".r
    pattern.matches(word) && !word.exists(_.isDigit)
  }

  def allLines(fileSpec: String): Future[Vector[String]] = Future {
    Source.fromFile(fileSpec, "ISO-8859-1").getLines().toVector
  }

  def criteria(lines: Vector[String]): Future[Vector[String]] = Future {
    lines.flatMap(_.split("[.,!?:;/\\s]+"))
      .filter(_.nonEmpty)
      .map(_.toLowerCase)
      .filter(wordCriteria)
  }

  def topKWords(k: Int)(fileSpec: String): Vector[(String, Int)] = {
    val linesFuture: Future[Vector[String]] = allLines(fileSpec)

    val wordsFuture = linesFuture.flatMap { lines =>
      val wordChunks = lines.grouped(50).toVector
      val chunkFutures = wordChunks.map(eachChunks => criteria(eachChunks))
      Future.sequence(chunkFutures).map { wordLists =>
        val sameWord: Map[String, Seq[String]] = wordLists.flatten.groupBy(x => x)
        val allCount: Map[String, Int] = sameWord.map { case (word, same) => (word, same.size) }
        val sorted = allCount.toVector.sortWith { (a, b) =>
          if (a._2 == b._2) a._1 < b._1
          else a._2 > b._2
        }
        sorted.take(k)
      }
    }
    Await.result(wordsFuture, Duration.Inf)
  } 

  def main(args: Array[String]): Unit = {
    val fileSpec = "C:/MUIC/funpar/Assignment/a3/test.txt"
    val res1 = topKWords(20)(fileSpec)
    println(res1)
  }
}
