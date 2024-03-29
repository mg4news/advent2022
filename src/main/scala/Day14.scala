import scala.annotation.tailrec
import Point._

object Day14 extends DayX(14)  {
  private def nextPoint(pnt: Point, grid: Array[Array[Char]], xoff: Int): Option[Point] =
    val base = Vector(Point(pnt.x, pnt.y + 1), Point(pnt.x - 1, pnt.y + 1), Point(pnt.x + 1, pnt.y + 1))
    val nextOk = for
      p <- base
      if grid(p.x - xoff)(p.y) == '.'
    yield p
    val nextFall = for
      p <- base
      if grid(p.x - xoff)(p.y) == '~'
    yield p
    if (nextOk.nonEmpty) Some(nextOk.head) else if (nextFall.nonEmpty) Option(nextFall.head) else None

  private case class Line(s: Point, e: Point) {
    override def toString: String = "l(" + s + "->" + e + ")"
    def isVertical: Boolean = s.x == e.x
  }

  private def toLine(s: String): IndexedSeq[Line] =
    val as = s.split(" -> ")
    for
      i <- 1 until as.length
      s = as(i-1).split(",")
      e = as(i).split(",")
    yield Line(Point(s(0).toInt,s(1).toInt), Point(e(0).toInt,e(1).toInt))

  // Need to account for the (500, 0) point
  private def getLimits(lines: List[Line]): (Point, Point) =
    Point.getLimits(Point(500,0) :: lines.flatMap(l => List(l.s, l.e)))

  @tailrec
  private def gridAddLines(grid: Array[Array[Char]], xoff: Int, lines: List[Line]): Unit =
    if (lines.nonEmpty) {
      val l = lines.head
      if (l.isVertical) {
        val ymin = l.s.y.min(l.e.y)
        val ymax = l.s.y.max(l.e.y)
        for (y <- ymin to ymax) grid(l.s.x - xoff)(y) = '#'
      } else {
        val xmin = l.s.x.min(l.e.x)
        val xmax = l.s.x.max(l.e.x)
        for (x <- xmin to xmax) grid(x - xoff)(l.s.y) = '#'
      }
      gridAddLines(grid, xoff, lines.tail)
    }

  @tailrec
  private def dropSand(grid: Array[Array[Char]], xoff: Int, origin: Point): Point = nextPoint(origin, grid, xoff) match
    case Some(p) => if (grid(p.x-xoff)(p.y) == '~') p else dropSand(grid, xoff, p)
    case None => origin

  @tailrec
  private def fillSand(grid: Array[Array[Char]], xoff: Int, origin: Point, acc: Int = 0): Int =
    val d = dropSand(grid, xoff, origin)
    if ((d == origin) || (grid(d.x-xoff)(d.y) == '~')) acc else {
      grid(d.x-xoff)(d.y) = 'o'
      fillSand(grid, xoff, origin, acc+1)
    }

  private def part1(ll: List[Line]): Unit =
    println()
    println("Part 1:")

    // allow a boundary left, right and below the actual map. Once sand hits the boundary it drops, and w are done
    val (pmin, pmax) = getLimits(ll)
    val pdim = pmax.sub(pmin)
    val xoff = pmin.x - 1 // i.e. idx=0
    val xdim = pdim.x + 3 // allow for left and right "fall line" =1 for size
    val ydim = pdim.y + 2 // allow for a bottom "fall line" +1 for size
    val grid = Array.fill[Char](xdim, ydim)('.')
    println("grid size = " + xdim + " x " + ydim)
    println("x offset  = " + xoff)
    grid(500 - xoff)(0) = '+'
    for (x <- 0 until xdim) grid(x)(ydim - 1) = '~'
    for (y <- 0 until ydim) {
      grid(0)(y) = '~'
      grid(xdim - 1)(y) = '~'
    }

    // insert lines, fill with sand
    gridAddLines(grid, xoff, ll)
    AdventUtils.drawGrid(grid,false)
    println()
    println("sand = " + fillSand(grid, xoff, Point(500, 0)))
    println()
    AdventUtils.drawGrid(grid,false)

  private def part2(ll: List[Line]): Unit =
    println()
    println("Part 2:")

    // add a line of rock two below
    // sand falls evenly diagonally, So new width needs to be 2*height, centered at 500,0
    val (pmin, pmax) = getLimits(ll)
    val pdim = pmax.sub(pmin)
    val ydim = pdim.y + 3 // extra 2, then +1 for size
    val xdim = ydim*2
    val xoff = 500-ydim
    val grid = Array.fill[Char](xdim, ydim)('.')
    println("grid size = " + xdim + " x " + ydim)
    println("x offset  = " + xoff)
    grid(500 - xoff)(0) = '+'
    for (x <- 0 until xdim) grid(x)(ydim - 1) = '#'
    gridAddLines(grid, xoff, ll)
    AdventUtils.drawGrid(grid,false)
    println()
    println("sand = " + (fillSand(grid, xoff, Point(500, 0)) + 1)) // add 1 to account for standing above 500,0
    println()
    AdventUtils.drawGrid(grid,false)

  override def runner(): Unit =
    val ll = lineList.flatMap(s => toLine(s))
    part1(ll)
    part2(ll)
}
