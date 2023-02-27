import scala.annotation.tailrec

object Day9 extends DayX(9) {

  case class Coord(x: Int,y: Int) {
    override def toString: String = "(" + x + "," + y + ")"
    def add(c: Coord): Coord = Coord(x+c.x, y+c.y)
    def follow(head: Coord): Coord =
      val dx = head.x - x
      val dy = head.y - y
      if dx.abs > 1 || dy.abs > 1 then Coord(x+dx.sign, y+dy.sign)
      else this
  }
  object Coord {
    def move(s: String): Coord = s match
      case "U" => Coord(0, 1)
      case "D" => Coord(0, -1)
      case "L" => Coord(-1, 0)
      case "R" => Coord(1, 0)
  }

  def buildRope(size: Int): List[Coord] = size match
    case 0 => List()
    case _ => Coord(0, 0) :: buildRope(size - 1)

  def ropeFollow(head: Coord, rope: List[Coord]): List[Coord] = rope match
    case Nil => List()
    case knot::_ =>
      val next = knot.follow(head)
      next :: ropeFollow(next, rope.tail)

  def moveN(c: Coord, steps: Int): List[Coord] =
    if (steps > 0) c :: moveN(c, steps - 1) else List()

  def movesToDiffs(ls: List[String]): List[Coord] = ls match
    case h :: _ =>
      val p = h.split(" ")
      moveN(Coord.move(p(0)), p(1).toInt) ::: movesToDiffs(ls.tail)
    case Nil => List()

  def applyAllDiffs(len: Int, diffs: List[Coord]): Set[Coord] =
    @tailrec
    def helper(r: List[Coord], d: List[Coord], s: Set[Coord]): Set[Coord] = d match
      case Nil => s
      case h::_ =>
        val head = r.head.add(h)
        val newrope = head :: ropeFollow(head, r.tail)
        helper(newrope, d.tail, s + newrope(len-1))
    val rope = buildRope(len)
    helper(rope, diffs, Set(Coord(0,0)))

  override def runner(): Unit =
    val diffs = movesToDiffs(lineList)
    val unique = applyAllDiffs(10, diffs)
    println("size of unique = " + unique.size)
}
