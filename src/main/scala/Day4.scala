object Day4 extends DayX(4):

  case class StrRange (s: String):
    private val as = s.split("-")
    val i1: Int = as(0).toInt
    val i2: Int = as(1).toInt

    def contains(cr: StrRange): Boolean =
      i1 <= cr.i1 && i2 >= cr.i2

    def overlaps(cr: StrRange): Boolean =
      ((cr.i1 >= i1) && (cr.i1 <= i2)) || (cr.i2 >= i1 && cr.i2 <= i2)

    override def toString: String = "[" + i1 + "," + i2 + "]"

  def result1(cr1: StrRange, cr2: StrRange): Int = if (cr1.contains(cr2) || cr2.contains(cr1)) 1 else 0
  def result2(cr1: StrRange, cr2: StrRange): Int = if (cr1.overlaps(cr2) || cr2.overlaps(cr1)) 1 else 0

  // the runner
  override def runner(ls: List[String]): Unit =
    val pairs = ls.map(s => s.split(",").toList.map(s => StrRange(s)))
    val res1 = pairs.map(l => result1(l.head, l(1)))
    println("sum1 = " + res1.sum)
    val res2 = pairs.map(l => result2(l.head, l(1)))
    println("sum1 = " + res2.sum)

