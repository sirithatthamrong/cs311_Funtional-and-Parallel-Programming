import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object DataCrunch {

  trait DataProvider {
    def get(onSuccess: Seq[String] => Unit,
            onFailure: () => Unit): Unit
  }

  object LoremIpsum extends DataProvider {
    override def get(onSuccess: Seq[String] => Unit,
            onFailure: () => Unit): Unit = {
      val lorem =
        """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        Cras nec sagittis justo. Nullam dignissim ultricies velit a tempus.
        Aenean pharetra semper elit eu luctus. Fusce maximus lacus eget magna
        ultricies, ac suscipit justo lobortis. Nullam pellentesque lectus
        at tellus gravida rhoncus. Nam augue tortor, rutrum et eleifend id,
        luctus ut turpis. Vivamus nec sodales augue.

        Suspendisse non erat diam. Mauris hendrerit neque at sem laoreet
          vehicula. Sed aliquam urna a efficitur tincidunt. In non purus
        tincidunt, molestie velit vulputate, mollis nisl. Pellentesque
        rhoncus molestie bibendum. Etiam sit amet felis a orci fermentum
        tempor. Duis ante lacus, luctus ac scelerisque eget, viverra ut felis."""
      onSuccess(lorem.split("\n"))
    }
  }

  object FailedSample extends DataProvider {
    override def get(onSuccess: Seq[String] => Unit,
                     onFailure: () => Unit): Unit = {
      onFailure()
    }
  }

  // This returns a DataProvider that feeds the "consumer" all the lines from a
  // file indicated by filename.
  def FileSource(filename: String): DataProvider = new DataProvider {
    override def get(onSuccess: Seq[String] => Unit, onFailure: () => Unit): Unit = {
      try {
        val lines = io.Source.fromFile(filename)
          .getLines
          .toVector
        onSuccess(lines)
      }
      catch {
        case _: Throwable => onFailure()
      }
    }
  }

  def dataProviderFuture(dp: DataProvider): Future[Seq[String]] = {
    val p = Promise[Seq[String]]
    dp.get(line => p.success(line), () => p.failure(new Exception("failed")))
    p.future
  }

  def highestFreq(linesFut: Future[Seq[String]]): Future[(String, Double)] = {
    linesFut.map { line =>
      val words: Seq[String] = line.flatMap(_.split("\\s+")).filter(_.nonEmpty)
//      println(words)
      if (words.nonEmpty) {
        val sameWord: Map[String, Seq[String]] = words.groupBy(x => x)
        val allCount: Map[String, Double] = sameWord.map { case (word, same) => (word, same.size.toDouble) }
        val mostFreqWord: String = allCount.maxBy(_._2)._1
        val popularFrac = allCount(mostFreqWord) / words.size
        (mostFreqWord, popularFrac)
      }
      else {
        ("", 0.0)
      }
    }
  }

  def main(args: Array[String]) = {
    // Example of how DataProvider is typically used. Comment out the block of code
    // below so it doesn't print some random garbage.
    def funcOnSuccess(lines: Seq[String]) = lines.foreach(println(_))
    def funcOnFailure() = println("Failed")
    val sampleProvider = LoremIpsum
    sampleProvider.get(funcOnSuccess, funcOnFailure)

    val res1 = highestFreq(dataProviderFuture(sampleProvider))
    Await.result(res1, Duration.Inf)
    println(res1) // Future(Success((a,0.02912621359223301)))
  }
}
