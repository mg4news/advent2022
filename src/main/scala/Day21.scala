import scala.annotation.tailrec

object Day21 extends DayX(21) {

  private def op(l: Long, r: Long, o: String): Long = o match
    case "+" => l+r
    case "/" => l/r
    case "-" => l-r
    case "*" => l*r

  // acc = the "previous" accumulator, r = the result of the "other" tree, left is true if the left branch contains "humn"
  private def opInverse(acc: Long, r: Long, isLeft: Boolean, o: String): Long = o match
    case "+" => acc-r
    case "-" => if (isLeft) acc+r else r-acc
    case "/" => if (isLeft) acc*r else r/acc
    case "*" => acc/r

  private trait Monkey
  private case class MonkeyVal(value: Long) extends Monkey
  private case class MonkeyOp(left: String, right: String, op: String) extends Monkey

  private val MonkeyValRegex = raw"([a-z]{4}): ([0-9]+)".r
  private val MonkeyOpRegex = raw"([a-z]{4}): ([a-z]{4}) ([-+*/]) ([a-z]{4})".r
  private def stringToMonkeyPair(s: String): (String, Monkey) = s match
    case MonkeyValRegex(name, value) => (name, MonkeyVal(value.toLong))
    case MonkeyOpRegex(name, left, op, right) => (name, MonkeyOp(left, right, op))

  private def resolveFrom(mm: Map[String, Monkey], name: String): Long = mm(name) match
    case MonkeyVal(v) => v
    case MonkeyOp(l, r, o) => op(resolveFrom(mm, l), resolveFrom(mm, r), o)

  private def hasHuman(mm: Map[String, Monkey], name: String): Boolean =
    if (name == "humn") true else mm(name) match
      case MonkeyVal(v) => false
      case MonkeyOp(l, r, _) => hasHuman(mm, l) || hasHuman(mm, r)

  @tailrec
  private def resolveForHumn(mm: Map[String, Monkey], from: String, accumulator: Long): Long =
    def step(l: String, r: String, o: String): (String, Long) =
      val isLeft = hasHuman(mm, l)
      val nextAcc = if (isLeft) opInverse(accumulator, resolveFrom(mm,r), isLeft, o) else opInverse(accumulator, resolveFrom(mm, l), isLeft, o)
      if (isLeft) (l, nextAcc) else (r, nextAcc)
    if (from == "humn") accumulator else mm(from) match
      case MonkeyOp(l, r, o) =>
        val (nfrom, nacc) = step(l, r, o)
        resolveForHumn(mm, nfrom, nacc)
      case MonkeyVal(_) => 0

  def runner(): Unit =
    val mm = lineList.map(s => stringToMonkeyPair(s)).toMap
    println("part1 = " + resolveFrom(mm, "root"))

    val root = mm("root").asInstanceOf[MonkeyOp]
    val humn = if (hasHuman(mm, root.left)) resolveForHumn(mm, root.left, resolveFrom(mm, root.right)) else resolveForHumn(mm, root.right, resolveFrom(mm, root.left))
    println("part2: humn shouts = " + humn)
}
