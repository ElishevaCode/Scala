val sentences = List("hello I am Itai", "hello hello", "hello I am Itai", "bbb ccc")
def splitSentenceToWords(s: String): List[String] = s.split(" ").toList

val listOfLIstOfWords = sentences.map(splitSentenceToWords)

val words = listOfLIstOfWords.flatten

val mapped = words.map(word => Map(word -> 1))

def mergeWordCounts(one: Map[String, Int], other: Map[String, Int]): Map[String, Int] = {
  println(s"one = ${one}")
  println(s"other = ${other}")
  println("")
  one.toList
    .foldLeft(other) { (agg, kv) =>
      val (k, v) = kv
      agg.updated(
        key = k,
        value = agg.get(k).map(_ + v).getOrElse(v)
      )
    }
}


import Ordering.Int
val wordCounts = mapped.reduce(mergeWordCounts)
  .toList.sortBy(_._2).reverse

List(1, 2, 3, 4).sum  // .sum is same as // .reduce(_ + _)

/* unit test example

test("should calc word count") {

  val expectedResult: Map[String, Int] =
    Map("hello" -> 2,
         "I" -> 2,
         "am" -> 3) // ...
  val sentences = List("hello I am Itai", "hello I am Itai", "bbb ccc")

  AssertEquals(wordCount(sentences),  expectedResult)
}

 */

