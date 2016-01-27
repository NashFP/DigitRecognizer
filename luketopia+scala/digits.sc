import scala.io.Source

type Pixels = Array[Int]

case class Digit(value: Int, pixels: Pixels)

def classifyInput(trainingDigits: Array[Digit], input: Pixels): Int = {
  def distance(pixels1: Pixels, pixels2: Pixels) =
    pixels1.zip(pixels2)
      .map(p => (p._1 - p._2) * (p._1 - p._2))
      .sum

  trainingDigits
    .minBy(digit => distance(digit.pixels, input))
    .value
}

def readDigits(file: String): Array[Digit] = {
  def readDigit(line: String) = {
    val entry = line.split(',').map(_.toInt)
    Digit(entry.head, entry.tail)
  }

  Source.fromFile(file)
    .getLines
    .drop(1)
    .map(readDigit)
    .toArray
}

def printResults(): Unit = {
  case class Pair(expected: Int, actual: Int)

  val trainingDigits = readDigits("training-sample.csv")
  val testDigits = readDigits("test-sample.csv")
  val classify = classifyInput(trainingDigits, _)

  val pairs = testDigits.map { digit =>
    val pair = Pair(digit.value, classify(digit.pixels))
    println(pair)
    pair
  }

  val successRate = pairs
    .count(pair => pair.expected == pair.actual)
    ./(pairs.length.toFloat)
    .*(100)

  println(s"Success Rate: $successRate%")
}

printResults()
