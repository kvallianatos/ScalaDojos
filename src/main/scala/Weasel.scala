import scala.annotation.tailrec
import scala.util.Random

object Weasel extends App {

  val target = "METHINKS IT IS LIKE A WEASEL"

  val genePool: List[String] = List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
    "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", " ")

  private def getSurvivalScore(mutation: String): Int = {
    mutation.zip(target).count(x => x._1 == x._2)
  }

  private def evolve(mutations: List[String], survivalFunc: String => Int): String = {
    mutations.map(x => (x, survivalFunc(x)))
      .sortBy(_._2).last._1
  }

  private def procreate(survivor: String): List[String] = {
    List.fill(100)(mutate(survivor))
  }

  private def mutate(input: String): String = {
    input.toSeq.map(x =>
      if(Random.nextInt(19) == 10) genePool(Random.nextInt(genePool.size))
      else x
    ).mkString
  }

  private def createPrimordialSoup: String = {
    List.fill(target.length)(genePool(Random.nextInt(genePool.size))).mkString
  }

  @tailrec
  private def simulateLyf(organism: String, generation: Int): Int = {
    if(organism equals target) generation
    else simulateLyf(evolve(procreate(organism),getSurvivalScore), generation + 1)
  }

  val numgen = simulateLyf(createPrimordialSoup, 0)
  println(numgen)
}
