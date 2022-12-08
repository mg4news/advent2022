import scala.annotation.tailrec

object Day7 extends DayX(7) {

  private def isCmd(s: String): Boolean = if (s.toList.head == '$') true else false
  private def isLS(s: String): Boolean = if (s == "$ ls") true else false

  @tailrec
  private def backOneDir(lc: List[Char]): List[Char] =
    val last = lc.length - 1
    if (lc(last) != '/') backOneDir(lc.take(last)) else lc.take(last)

  private def updateDir(cmd: String, cwd: String): String =
    val p = cmd.split(" ")
    if (p(2) == "..")
      backOneDir(cwd.toList).mkString
    else if (p(2) == "/")
      "/"
    else if (cwd == "/")
      cwd + p(2)
    else cwd + "/" + p(2)

  private def elemValue(s: String): Long =
    val p = s.split(" ")
    if (p(0) == "dir") 0 else p(0).toLong

  @tailrec
  private def dirSize(ls: List[String], acc: Long = 0): (Long, List[String]) = ls match
    case h::_ =>
      if (isCmd(h)) {
        (acc, ls)
      }
      else dirSize(ls.tail, acc + elemValue(h))
    case Nil => (acc,List())

  case class Info(dir: String, size: Long) {
    override def toString: String = dir + " " + size
    def contains(i: Info): Boolean = dir.startsWith(i.dir)
    def add(i: Info): Info = Info(dir, size + i.size)
  }

  implicit val infoOrdering: Ordering[Info] = Ordering.by(_.dir)

  @tailrec
  def walker(ls: List[String], cwd:String = " ", lacc: List[Info] = List()): List[Info] = ls match
    case h::_ =>
      if (isCmd(h) && isLS(h)) {
        val n = dirSize(ls.tail)
        walker(n._2, cwd, Info(cwd, n._1)::lacc)
      }
      else if (isCmd(h)) walker(ls.tail, updateDir(ls.head, cwd), lacc)
      else walker(ls.tail, cwd, lacc)
    case Nil => lacc

  private def sumDirs(le: List[Info]): List[Info] =
    val ar = le.toArray
    for (i <- ar.indices) {
      for (j <- i+1 until ar.length) {
        if (ar(j).contains(ar(i))) ar(i) = ar(i).add(ar(j))
      }
    }
    ar.toList

  @tailrec
  private def sumElems(le: List[Info], acc: Long = 0): Long = le match
    case h::_ => sumElems(le.tail, acc + h.size)
    case Nil => acc

  @tailrec
  private def lteSize(le: List[Info], size: Long, prev: Long): Long = le match
    case  h::_ =>
      if ((h.size >= size) && (h.size < prev)) {
        lteSize(le.tail, size, h.size)
      } else lteSize(le.tail, size, prev)
    case Nil => prev

  override def runner(ls: List[String]): Unit =
    // To strings of Intem, sorted alphabetically
    val lc = walker(ls).sorted

    // get directory sums
    val lsum = sumDirs(lc)

    // Trim anything over limit, sum up what remains
    val ltrim = lsum.filter(_.size <= 100000)
    println("sum = " + sumElems(ltrim))

    // part 2: space calculation
    val space: Long = 70000000 - lsum.head.size
    val need: Long = 30000000 - space
    println("space = " + space)
    println("need = " + need)
    println("delete = " + lteSize(lsum, need, 70000000))
}
