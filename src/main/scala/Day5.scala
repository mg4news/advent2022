import scala.annotation.tailrec

object Day5 extends DayX(5) {
  // Cop out, Faster than parsing out the info.
  val stack1: List[Char] = List('T','R','D','H','Q','N','P','B')
  val stack2: List[Char] = List('V','T','J','B','G','W')
  val stack3: List[Char] = List('Q','M','V','S','D','H','R','N')
  val stack4: List[Char] = List('C','M','N','Z','P')
  val stack5: List[Char] = List('B','Z','D')
  val stack6: List[Char] = List('Z','W','C','V')
  val stack7: List[Char] = List('S','L','Q','V','C','N','Z','G')
  val stack8: List[Char] = List('V','N','D','M','J','G','L')
  val stack9: List[Char] = List('G','C','Z','F','M','P','T')

  case class Instruction(ins: String):
    val li: Array[String] = ins.split(" ")
    val num: Int = li(1).toInt
    val src: Int = li(3).toInt - 1
    val dst: Int = li(5).toInt - 1

    override def toString: String = "move " + num + " from " + (src+1) + " to " + (dst+1)

    private def doMove(lls: List[List[Char]]): (List[Char],List[Char]) =
      val d = lls(src).take(num).reverse
      (lls(src).drop(num), d:::lls(dst))

    private def doMovePreserve(lls: List[List[Char]]): (List[Char], List[Char]) =
      val d = lls(src).take(num)
      (lls(src).drop(num), d ::: lls(dst))

    def runMe(lls: List[List[Char]], preserve: Boolean): List[List[Char]] =
      val res = if preserve then doMovePreserve(lls) else doMove(lls)
      def builder(acc: Int): List[List[Char]] =
        if acc < 0 then List()
        else if acc == src then res._1::builder(acc-1)
        else if acc == dst then res._2::builder(acc-1)
        else
          val l = lls(acc)
          l::builder(acc-1)
      // got to reverse to get things back in order
      builder(lls.length-1).reverse

  @tailrec
  def runInstructions(li: List[Instruction], st: List[List[Char]], preserve: Boolean) : List[List[Char]] = li match
      case h::_ =>
        runInstructions(li.tail, h.runMe(st, preserve), preserve)
      case _ => st

  override def runner(ls: List[String]): Unit =
    val lls = StringListHelper.splitByEmptyString(ls)
    val insts = lls(1).map(s => Instruction(s))
    val stacks = List(stack1, stack2, stack3, stack4, stack5, stack6, stack7, stack8, stack9)

    runInstructions(insts, stacks, false).foreach(s => print(s.head))
    println()
    runInstructions(insts, stacks, true).foreach(s => print(s.head))
    println()
}
