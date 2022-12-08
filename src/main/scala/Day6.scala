import scala.None
import scala.annotation.tailrec

object Day6 extends DayX (6) {

  @tailrec
  def dataOffset(lc: List[Char], hdr: Int, acc: Int = 0): Option[Int] =
    if lc.length <= hdr then None
    else if lc.take(hdr).toSet.size == hdr then Some(acc + hdr)
    else dataOffset(lc.tail, hdr, acc+1)

  override def runner(ls: List[String]): Unit =
    println("Data input = " + dataOffset(ls.head.toList, 4))
    println("Mesg input = " + dataOffset(ls.head.toList, 14))
}
