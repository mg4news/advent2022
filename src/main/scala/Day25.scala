object Day25 extends DayX(25):

  private val digit2int = Map('2' -> 2, '1' -> 1, '0' -> 0, '-' -> -1, '=' -> -2)
  private val int2digit = digit2int.map(_.swap)

  private def deSnafu(s: String): Long = 
    def helper(cl: List[Char]): Long = cl match
      case h::_ => digit2int(h) * scala.math.pow(5, cl.length-1).toLong + helper(cl.tail)
      case _ => 0
    helper(s.toList)

  private def enSnafu(num: Long): String =
    val inverse = Iterator.unfold(num) {v =>
      Option.when(v != 0) {
        val remainder = math.floorMod(v, 5).toInt
        val adjusted = if remainder > 2 then remainder-5 else remainder
        (int2digit(adjusted), (v - adjusted) / 5)
      }
    }
    if inverse.isEmpty then "0" else inverse.mkString.reverse

  private val snums = lineList
  //private val snums = FileFactory.toLineList("day25test.txt").getOrElse(List())

  override def runner(): Unit =
    val num = snums.foldLeft(0.toLong) {(x, s) => x + deSnafu(s)}
    println("total = " + num)
    println("snafu = " + enSnafu(num))

end Day25

