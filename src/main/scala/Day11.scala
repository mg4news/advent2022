import scala.annotation.tailrec

object Day11 extends DayX(11) {

  sealed trait MonkeyOp {
    def op(old: Long): Long
  }
  case object OldByOld extends MonkeyOp {
    override def op(old: Long): Long = old*old
  }
  case class OldTimesInt(num: Long) extends MonkeyOp {
    override def op(old: Long): Long = old * num
  }
  case class OldAddInt(num: Long) extends MonkeyOp {
    override def op(old: Long): Long = old + num
  }

  private def getThrow(s: String): (Boolean, Int) = s match
    case s"    If true: throw to monkey $monkey" => (true, monkey.toInt)
    case s"    If false: throw to monkey $monkey" => (false, monkey.toInt)

  private def getTest(s: String): Long = s match
    case s"  Test: divisible by $divisor" => divisor.toLong

  private def getOp(s: String): MonkeyOp = s match
    case "  Operation: new = old * old" => OldByOld
    case s"  Operation: new = old * $num" => OldTimesInt(num.toInt)
    case s"  Operation: new = old + $num" => OldAddInt(num.toInt)

  private def getItems(s: String): Array[Long] =
    s.split(": ")(1).split(", ").map(_.toInt)

  case class Monkey(id: Int, items: List[Long], op: MonkeyOp, tst: Long, onFalse: Int, onTrue: Int, inspects: Long = 0) {
    override def toString: String =
      "Monkey:" + id + ", " + items + ", div by " + tst + ", false->" + onFalse + ", true->" + onTrue + " inspects=" + inspects

    def roundBy3(): List[(Int, Long)] =
      for {
        i <- items
        worry = op.op(i) / 3
        next = if (worry % tst == 0) onTrue else onFalse
      } yield (next, worry)

    def roundAdjust(adjust: Long): List[(Int,Long)] =
      for {
        i <- items
        worry = op.op(i) % adjust
        next = if (worry % tst == 0) onTrue else onFalse
      } yield (next, worry)

    def update(w: Long): Monkey = Monkey(id, w::items ,op,tst,onFalse, onTrue, inspects)
    def throwAll(): Monkey = Monkey(id, List(), op, tst, onFalse, onTrue, inspects + items.length)
  }

  def monkeyMaker(ls: List[String]): Monkey =
      val numberRegex = "([0-9]+)".r
      val thrw: IndexedSeq[(Boolean, Int)] = for (i <- 4 to 5) yield getThrow(ls(i))
      Monkey(
        numberRegex.findFirstIn(ls.head).getOrElse("0").toInt,
        getItems(ls(1)).toList,
        getOp(ls(2)),
        getTest(ls(3)),
        thrw.find(x => !x._1).getOrElse((false,0))._2,
        thrw.find(x => x._1).getOrElse((true,0))._2,
    )

  @tailrec
  def throwToMonkeys(ml: List[Monkey], tl: List[(Int,Long)]): List[Monkey] = tl match
    case to::_ =>
      val mt = ml.drop(to._1)
      throwToMonkeys(ml.take(to._1) ::: (mt.head.update(to._2) :: mt.tail), tl.tail)
    case _ => ml

  @tailrec
  def oneRound(ms: List[Monkey], adjust: Long, id: Int = 0): List[Monkey] = id match
    case i if i < ms.length =>
      val tl = if (adjust == 0) ms(i).roundBy3() else ms(i).roundAdjust(adjust)
      val mt = ms.drop(i)
      oneRound(throwToMonkeys(ms.take(i) ::: (mt.head.throwAll() :: mt.tail), tl), adjust, id+1)
    case _ => ms

  @tailrec
  def doRounds(rounds: Int, ms: List[Monkey], adjust: Long): List[Monkey] = rounds match
    case r if r > 0 => doRounds(rounds-1, oneRound(ms, adjust), adjust)
    case _ => ms

  override def runner(ls: List[String]): Unit =
    val ms = ListHelpers.toNonEmpty(ls).grouped(6)
    val monkeys = (for m <- ms yield monkeyMaker(m)).toList

    val m1 = doRounds(20, monkeys, 0)
    val biz1 = m1.map(_.inspects).sorted.reverse
    println(" part1: level = " + biz1.take(2).product)

    val adjust = monkeys.map(_.tst).product
    val m2 = doRounds(10000, monkeys, adjust)
    val biz2 = m2.map(_.inspects).sorted.reverse
    println(" part 2: level = " + biz2.take(2).product)
}
