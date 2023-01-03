import scala.annotation.tailrec

object Day14 extends DayX(14)  {
  private case class Point(x: Int, y: Int) {
    override def toString: String = "p(" + x + "," + y + ")"
    def sub(p: Point): Point = Point(x-p.x, y-p.y)
    def next(grid: Array[Array[Char]], xoff: Int): Option[Point] =
      val base = Vector(Point(x,y+1), Point(x-1,y+1), Point(x+1,y+1))
      val nextOk = for
        p <- base
        if grid(p.x-xoff)(p.y) == '.'
      yield p
      val nextFall = for
        p <- base
        if grid(p.x-xoff)(p.y) == '~'
      yield p
      if (nextOk.nonEmpty) Some(nextOk.head) else if (nextFall.nonEmpty) Option(nextFall.head) else None
  }
  private case class Line(s: Point, e: Point) {
    override def toString: String = "l(" + s + "->" + e + ")"
    def isVertical: Boolean = s.x == e.x
    def sub(p: Point): Line = Line(s.sub(p), e.sub(p))
  }

  private def toLine(s: String): IndexedSeq[Line] =
    val as = s.split(" -> ")
    for
      i <- 1 until as.length
      s = as(i-1).split(",")
      e = as(i).split(",")
    yield Line(Point(s(0).toInt,s(1).toInt), Point(e(0).toInt,e(1).toInt))

  private def getLimits(lines: List[Line]): (Point, Point) =
    @tailrec
    def helper(lp: List[Point], minp: Point, maxp: Point): (Point, Point) = lp match
      case  h::_ =>
        val xmin = if (minp.x <= h.x) minp.x else h.x
        val ymin = if (minp.y <= h.y) minp.y else h.y
        val xmax = if (maxp.x >= h.x) maxp.x else h.x
        val ymax = if (maxp.y >= h.y) maxp.y else h.y
        helper(lp.tail, Point(xmin,ymin), Point(xmax,ymax))
      case Nil => (minp, maxp)
    val lp = lines.flatMap(l => List(l.s, l.e))
    helper(lp, Point(500,0), Point(500,0))

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
  private def dropSand(grid: Array[Array[Char]], xoff: Int, origin: Point): Point = origin.next(grid, xoff) match
    case Some(p) => if (grid(p.x-xoff)(p.y) == '~') p else dropSand(grid, xoff, p)
    case None => origin

  @tailrec
  private def fillSand(grid: Array[Array[Char]], xoff: Int, origin: Point, acc: Int = 0): Int =
    val d = dropSand(grid, xoff, origin)
    if ((d == origin) || (grid(d.x-xoff)(d.y) == '~')) acc else {
      grid(d.x-xoff)(d.y) = 'o'
      fillSand(grid, xoff, origin, acc+1)
    }

  private def drawGrid(grid: Array[Array[Char]], width: Int, height: Int): Unit =
    for (y <- 0 until height)
      for (x <- 0 until width)
        print(grid(x)(y))
      println

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
    //drawGrid(grid, xdim, ydim)
    //println()
    println("sand = " + fillSand(grid, xoff, Point(500, 0)))
    //println()
    //drawGrid(grid, xdim, ydim)

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
    //drawGrid(grid, xdim, ydim)
    //println()
    println("sand = " + (fillSand(grid, xoff, Point(500, 0)) + 1)) // add 1 to account for standing above 500,0
    //println()
    //drawGrid(grid, xdim, ydim)

  override def runner(ls: List[String]): Unit =
    val ll = ls.flatMap(s => toLine(s))
    part1(ll)
    part2(ll)
}
