object Day2 extends DayX(2):

  private val strategy1 = List(
    ("A X", 1 + 3),
    ("A Y", 2 + 6),
    ("A Z", 3 + 0),
    ("B X", 1 + 0),
    ("B Y", 2 + 3),
    ("B Z", 3 + 6),
    ("C X", 1 + 6),
    ("C Y", 2 + 0),
    ("C Z", 3 + 3)
  )

  private val strategy2 = List(
    // chooses rock
    ("A X", 3 + 0), // lose- scissors(3)
    ("A Y", 1 + 3), // draw - rock(1)
    ("A Z", 2 + 6), // win - paper(2)
    // chooses paper
    ("B X", 1 + 0), // lose - rock(1)
    ("B Y", 2 + 3), // draw - paper(2) 
    ("B Z", 3 + 6), // win- scissors(3)
    // chooses scissors
    ("C X", 2 + 0), // lose - paper(2)
    ("C Y", 3 + 3), // draw - scissors(3)
    ("C Z", 1 + 6)  // win - rock(1)
  )

  def score(s: String, strategy: List[(String, Int)]): Int =
    strategy.find(x => x._1 == s).getOrElse(("", 0))._2

  override def runner(): Unit =
    println("real score 1 = " + lineList.map(s => score(s, strategy1)).sum)
    println("real score 2 = " + lineList.map(s => score(s, strategy2)).sum)

