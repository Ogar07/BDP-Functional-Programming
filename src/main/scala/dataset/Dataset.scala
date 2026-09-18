package dataset

import dataset.util.Commit.Commit

import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import scala.math.Ordering.Implicits._

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {


  /** Q23 (4p)
   * For the commits that are accompanied with stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = {
    val (sum, count) = input.foldLeft((0L, 0)) { (acc, commit) =>
      commit.stats.fold(acc)(stats =>
        (acc._1 + stats.additions, acc._2 + 1)
      )
    }
    (sum / count).toInt
  }

  /** Q24 (4p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * NB!filename of a file is always defined.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]): (Int, Int) = {
    val dateFormat = new SimpleDateFormat("HH")
    dateFormat.setTimeZone(new SimpleTimeZone(0, "UTC")) // timezone used to extract the hour changes; we want UTC time

    val counts = input.foldLeft(Map.empty[Int, Int]) { (acc, commit) => // acc is the map
      val hour = dateFormat.format(commit.commit.committer.date).toInt
      val js = commit.files.count(file => file.filename.exists(_.endsWith(".js"))) // counts how many .js files were changed in that particular commit
      acc.updated(hour, acc.getOrElse(hour, 0) + js) // adds those JS files to that hour's running total
    }
    counts.maxBy(_._2) // return tuple with max js file changes; _._2 tells us to look at the second component of each tuple
  }

  /** Q25 (5p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String): (String, Int) = {
    input.filter(commit => commit.url.startsWith(s"https://api.github.com/repos/$repo/commits/"))
      .groupBy(_.commit.author.name)
      .map{case (name, numCommits) => (name, numCommits.length)}
      .maxBy(_._2)
  }

  /** Q26 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    val dateFormat = new SimpleDateFormat("yyyy")
    dateFormat.setTimeZone(new SimpleTimeZone(0, "UTC"))

    def repoName(url: String): String = {
      val prefix = "https://api.github.com/repos/"
      val start = prefix.length
      val end = url.indexOf("/commits/", start)
      url.substring(start, end)
    }

    input.filter(commit => dateFormat.format(commit.commit.committer.date) == "2019")
      .groupBy(commit => repoName(commit.url))
      .map{case (repo, commits) => (repo, commits.length)}
  }

  /** Q27 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   * NB!filename of a file is always defined.
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = { // should we consider case sensitivity??
    input.flatMap(_.files) // list of commits to list of files
      .flatMap(_.filename) // list of file names
      .filter(_.contains(".")) // only keeps file names with an extension
      .map(filename => filename.substring(filename.lastIndexOf('.') + 1)) // extract the extension
      .groupBy(x => x)
      .map{case (x, files) => (x, files.length)}
      .toList
      .sortBy(_._2)(Ordering[Int].reverse)
      .take(5)
  }


  /** Q28 (9p)
   *
   * A day has different parts:
   * morning 5 am to 12 pm (noon)
   * afternoon 12 pm to 5 pm.
   * evening 5 pm to 9 pm.
   * night 9 pm to 4 am.
   *
   * Which part of the day was the most productive in terms of commits ?
   * Return a tuple with the part of the day and the number of commits
   *
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   */
  def mostProductivePart(input: List[Commit]): (String, Int) = {
    val dateFormat = new SimpleDateFormat("HH")
    dateFormat.setTimeZone(new SimpleTimeZone(0, "UTC"))

    input.map{commit =>
        val hour = dateFormat.format(commit.commit.committer.date).toInt
        if (hour >= 5 && hour < 12) "morning"
        else if (hour >= 12 && hour < 17) "afternoon"
        else if (hour >= 17 && hour < 21) "evening"
        else "night"
      }
      .groupBy(x => x)
      .map {case (x, commits) => (x, commits.length)}
      .maxBy(_._2)
  }
}
