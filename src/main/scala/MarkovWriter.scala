import scala.io.Source
import scala.util.Random

object MarkovWriter extends App {

  def readTexts(ids: Seq[String]): Seq[String] = {
    ids.map(id => Source.fromFile(getClass.getResource(id).getPath).mkString)
  }

  def tokenise(input: String): Seq[String] = {
    input.split("\\s+")
  }

  def createWordTriplets(input: Seq[String]): List[(String, String, String)] = {
    input.filterNot("\"()".toSet).filter(removeNonWords).sliding(3).map(triplet => (triplet(0), triplet(1), triplet(2))).toList
  }

  def removeNonWords(input: String): Boolean = {
   !(input matches "^[0-9\\-\\.\\:]+$")
  }

  //val triplets = createWordTriplets(tokenise(readTexts(Seq("artOfWar.txt", "bible.txt", "iliad.txt", "machiavelli.txt", "wealth.txt", "antichrist.txt"))))
  val triplets: List[(String, String, String)] = readTexts(Seq("artOfWar.txt", "bible.txt", "iliad.txt", "machiavelli.txt", "wealth.txt"))
    .map(txt => tokenise(txt)).map(w => createWordTriplets(w)).flatten.toList

  def compose(prev: (String, String), length: Int): Unit = {
    if (length > 100 && prev._2.matches(".*\\.$")) print("\n\nEND")
    else {
      if (length%30 == 0) println
      val choices = triplets.filter(str => str._1.trim == prev._1.trim && str._2.trim == prev._2.trim).map(_._3).distinct
      val next = choices(Random.nextInt(choices.size))

      print(s"$next ")
      compose((prev._2, next), length + 1)
    }
  }

  val init = ("Love is")

  println(s"Created ${triplets.size} triplets")
  println
  print(init)

  val seed = init.split(" ").map(_.toLowerCase).toList
  compose((seed(0), seed(1)), 0)
}
