import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.*

def mergeWordCounts(one: Map[String, Int], other: Map[String, Int]): Map[String, Int] = {
  one.toList
    .foldLeft(other) { (agg, kv) =>
      val (k, v) = kv
      agg.updated(
        key = k,
        value = agg.get(k).map(_ + v).getOrElse(v)
      )
    }
}

val sentences = List("hello I am Itai", "hello hello", "hello I am Itai", "bbb ccc")
def splitSentenceToWords(s: String): List[String] = s.split(" ").toList

val listOfLIstOfWords = sentences.map(splitSentenceToWords)

val words = listOfLIstOfWords.flatten

val listOfFutures: List[Future[Map[String, Int]]] = words.map(word => Future(Map(word -> 1)))
val futureOfList: Future[List[Map[String, Int]]] = Future.sequence(listOfFutures)

val result: Future[List[(String, Int)]] = futureOfList.map(
  mapped => mapped.reduce(mergeWordCounts)
    .toList.sortBy(_._2).reverse
)

import scala.concurrent.duration.Duration

val actualResult: List[(String, Int)] = Await.result(result, Duration.Inf)




//
//import Ordering.Int
//import scala.concurrent.Future
//val wordCounts = mapped.reduce(mergeWordCounts)
//  .toList.sortBy(_._2).reverse
//
//List(1, 2, 3, 4).sum
//
//
//val s1 = Set(1,2,3)
//val s2 = s1.map(_ + 10)
//
//



