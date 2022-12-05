import scala.annotation.tailrec

object Day3 extends DayX(3):

  private val ord_a = 'a'.toInt
  private val ord_A = 'A'.toInt

  def char2Int(c: Char) : Int = if (c.isUpper) c.toInt - ord_A + 27 else c.toInt - ord_a + 1

  def inter(ls: List[Int]): Int =
     val len = ls.length
     val s1 = ls.take(len / 2).toSet
     val s2 = ls.drop(len / 2).toSet
     s1.intersect(s2).toList.head

  def badge(ls: List[List[Int]]): Int =
    val s = ls.head.toSet.intersect(ls(1).toSet).intersect(ls(2).toSet)
    s.toList.head

  def sumBadges(ls: List[List[Int]]) : Int =
    @tailrec
    def helper(l: List[List[Int]], acc: Int): Int = l match
      case _ ::_ => helper(l.drop(3), acc + badge(l))
      case _ => acc
    helper(ls, 0)

  override def runner(ls: List[String]): Unit =
    val lols = ls.map(l => l.toList.map(c => char2Int(c)))
    println("sum priorities = " + lols.map(i => inter(i)).sum)
    println("sum badges     = " + sumBadges(lols))


