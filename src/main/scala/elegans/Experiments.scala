package elegans

object Experiments {
  case class Experiment(mutations: Map[String, String], fates: Set[Seq[String]]) {

    override def toString: String = {
      val mutString = (mutations.map {
        case (k, v) => k + ": " + v
      }).mkString(", ")
      val fateString = fates.map(_.mkString("")).mkString(",")
      mutString + ", " + fateString
    }

  }

  def parseExperiments(fname: String): List[Experiment] = {

    val lines = scala.io.Source.fromFile(fname).getLines.toList
    val mutationKeys = lines(0).split(",").map(_.trim)
    val experimentLines = lines.drop(1)

    val exs = for (line <- experimentLines;
         splitted = (line.split(",") map (_.trim)).toList) yield {
      val mutationValues = splitted.take(mutationKeys.size)
      val outcomeStrings = splitted.drop(mutationKeys.size)
      val mutations = (mutationKeys zip mutationValues).toMap
      val fates = outcomeStrings.map(str => str.toSeq.map(char => char.toString)).toSet
      Experiment(mutations, fates)
    }

    exs.toList
  }
}
