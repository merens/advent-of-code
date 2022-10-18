package aoc2015

object Day14 {

  case class Reindeer(name: String, speed: Int, activeTime: Int, restingTime: Int)

  def parseInput(input: String): Seq[Reindeer] = input.linesIterator.toSeq
    .map {
      // Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
      case s"$name can fly $speed km/s for $activeTime seconds, but then must rest for $restingTime seconds." =>
        Reindeer(name, speed.toInt, activeTime.toInt, restingTime.toInt)
      case _                                                                                                  => throw new IllegalArgumentException("parsing error")
    }

  def calculateDistance(time: Int)(reindeer: Reindeer): Int = {
    val cycleTime = reindeer.activeTime + reindeer.restingTime
    val timesActive: Int = time / cycleTime
    val remainder: Int = time % cycleTime
    reindeer.speed * (timesActive * reindeer.activeTime + Math.min(remainder, reindeer.activeTime))
  }

  def calculateLeader(time: Int)(reindeers: Seq[Reindeer]): Int = {
    val distances = reindeers.map(calculateDistance(time))
    distances.indexOf(distances.max)
  }

  def winnerPoints(time: Int)(reindeers: Seq[Reindeer]): Int =
    (1 to time)
      .map(second => calculateLeader(second)(reindeers))
      // from sequence to map of frequencies: https://stackoverflow.com/a/52678005
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .max

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(parseInput(input).map(calculateDistance(2503)).max)
    println(winnerPoints(2503)(parseInput(input)))
  }
}
