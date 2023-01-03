import scala.annotation.tailrec
import scala.collection.immutable.List

object Day20 extends DayX(20) {

  private def mix(arr: Array[(Long,Int)]): List[(Long,Int)] =
    val len = arr.length

    @tailrec
    def bubble(idx: Int, count: Long, up: Boolean): Unit = if (count > 0) {
      val next = if (up) {
        if (idx + 1 < len) idx+1 else 0
      } else {
        if (idx - 1 >= 0) idx-1 else len-1
      }
      val tmp = arr(next)
      arr(next) = arr(idx)
      arr(idx) = tmp
      bubble(next, count-1, up)
    }

    for (idx <- 0 until len) {
      val curr = arr.indexWhere(_._2 == idx)
      // do nothing for 0
      val newValue: Unit = if (arr(curr)._1 < 0)
        bubble(curr, arr(curr)._1.abs % len, false)
      if (arr(curr)._1 > 0)
        bubble(curr, arr(curr)._1 % len, true)
      else newValue
    }
    arr.toList

  case class Data1(li: List[Long]) {
    private val indexed = li.zipWithIndex
    private val mixed: List[(Long, Int)] = mix(indexed.toArray)

    def refNumber(offset: Int): Long =
      val idx = (mixed.indexWhere(_._1 == 0) + offset) % mixed.length
      mixed(idx)._1
  }

  case class Data2(li: List[Long]) {
    val indexed: Seq[(Long, Int)] = li.map(_ * 811589153).zipWithIndex

    val mixed: List[(Long, Int)] =
      val mixme = indexed.toArray
      for (i <- 1 to 10) {
        mix(mixme)
      }
      mixme.toList

    def refNumber(offset: Int): Long =
      val idx = (mixed.indexWhere(_._1 == 0) + offset) % mixed.length
      mixed(idx)._1
  }

  override def runner(ls: List[String]): Unit =
    val data1 = Data1(ls.map(_.toLong))
    val part1 = data1.refNumber(1000)
      + data1.refNumber(2000)
      + data1.refNumber(3000)
    println("Part 1 = " + part1)
    val data2 = Data2(ls.map(_.toLong))
    val part2 = data2.refNumber(1000)
      + data2.refNumber(2000)
      + data2.refNumber(3000)
    println("Part 2 = " + part2)
}
