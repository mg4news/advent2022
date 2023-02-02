import scala.annotation.tailrec

object Day15 extends DayX(15):

  private case class Element(sensor: Point, beacon: Point, manDist: Int, minRow: Int, maxRow: Int)

  private def stringToSigBeacon(s: String): (Point, Point) = s match
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
      (Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))

  private def getImpactedRows(sensor: Point, beacon: Point): (Int, Int,Int) =
    val dist = sensor.distManhattan(beacon)
    (dist, sensor.y - dist, sensor.y + dist)

  private def buildElement(s: String): Element =
    val pp = stringToSigBeacon(s)
    val ar = getImpactedRows(pp._1, pp._2)
    Element(pp._1, pp._2, ar._1, ar._2, ar._3)

  // Calculates the first and last absolute x coordinates of the row. anything outside of these are not impacted
  // and of the sensors or beacons
  @tailrec
  private def rowFirstLast(lsb: List[Element], rowIdx: Int, xmin: Int = 0, xmax: Int = 0): (Int, Int) = lsb match
    case h::_ =>
      val ydist = h.manDist - (h.sensor.y - rowIdx).abs
      rowFirstLast(lsb.tail, rowIdx, xmin.min(h.sensor.x - ydist), xmax.max(h.sensor.x + ydist))
    case Nil => (xmin,xmax)

  private def onlyImpactsSpecificRow(lsb: List[Element], row: Int): List[Element] =
    lsb.filter(e => e.minRow <= row && e.maxRow >= row)

  private def updateOne(row: Array[Char], xoff: Int, rowIdx: Int, sensor: Point, dist: Int): Unit =
    val xdist = dist - (rowIdx - sensor.y).abs
    for (x <- sensor.x-xdist to sensor.x+xdist)
      if (row(x-xoff) == '.') row(x-xoff) = '#'

  private def fillManhattan(elem: Element, grid: Grid[Char]): Unit =
    val lp = for {
      x <- (elem.sensor.x - elem.manDist) to (elem.sensor.x + elem.manDist)
      y <- (elem.sensor.y - elem.manDist) to (elem.sensor.y + elem.manDist)
      if elem.sensor.distManhattan(Point(x, y)) <= elem.manDist
    } yield Point(x, y)
    for (p <- lp) if grid(p) == '.' then grid(p) = '#'

  private def hasUndetectedBeacon(info: List[Element], rowIdx: Int): Option[Point] =
    @tailrec
    def gapInSpans(spans: List[(Int,Int)], prev: (Int, Int)): Option[Point] = spans match
      case s::_ =>
        if (s._1 - prev._2) > 1 then
          println("spans = " + prev + ", " + s)
          Some(Point(s._1-1, rowIdx))
        else
          val next = if s._2 > prev._2 then s else prev
          gapInSpans(spans.tail, next)
      case _    => None
    def elemToSpan(e: Element): (Int, Int) =
      val xdist = e.manDist - (rowIdx - e.sensor.y).abs
      (e.sensor.x - xdist, e.sensor.x + xdist)
    val relevant = onlyImpactsSpecificRow(info, rowIdx)
      .map(e => elemToSpan(e))
      .sortBy(s => s._1)
    gapInSpans(relevant, relevant.head)

  @tailrec
  private def rowParser(info: List[Element], curr: Int, end: Int): Option[Point] =
    //if curr >= end then None
    //else hasUndetectedBeacon(info, curr) match
    //  case Some(p) => Some(p)
    //  case None    => rowParser(info, curr+1, end)
    if curr >= end then None
    else hasUndetectedBeacon(info, end) match
      case Some(p) => Some(p)
      case None    => rowParser(info, curr, end-1)


  private def drawAll(info: List[Element]): Unit =
    val limits = info.flatMap(e => List(e.sensor.add(Point(e.manDist, e.manDist)), e.sensor.sub(Point(e.manDist, e.manDist))))
    val grid = Grid[Char](limits, Some('.'))
    println(grid)
    for (e <- info)
      grid(e.sensor) = 'S'
      grid(e.beacon) = 'B'
      fillManhattan(e, grid)
    grid.show()
    println

  private def pointToFreq(p: Point): Long =
    p.x.toLong * 4_000_000 + p.y

  private def part1(info: List[Element], rowIdx: Int): Unit =
    val relevant = onlyImpactsSpecificRow(info, rowIdx)
    val (xmin, xmax) = rowFirstLast(relevant, rowIdx)
    //println("row size = " + (xmax - xmin + 1) + " (" + xmin + " to " + xmax + ")")
    val row = Array.fill[Char](xmax - xmin + 1)('.')
    for (r <- relevant) {
      if (r.sensor.y == rowIdx) row(r.sensor.x - xmin) = 'S'
      if (r.beacon.y == rowIdx) row(r.beacon.x - xmin) = 'B'
    }
    for (r <- relevant) {
      updateOne(row, xmin, rowIdx, r.sensor, r.manDist)
    }
    var count = 0
    for (i <- row.indices)
      if (row(i) == '#') count = count + 1
    println("Part 1: count = " + count)

  override def runner(ls: List[String]): Unit =
    val info = ls.map(s => buildElement(s))
    val testData = if info.length < 20 then true else false

    // for the test case draw the grid. Will just skip the real array
    // because its much bigger than 20
    if testData then
      drawAll(info)
      part1(info, 10)
    else
      part1(info, 2000000)

    // part 2
    val beacon = if testData then
      rowParser(info, 0, 20)
    else
      rowParser(info, 0, 4000000)
    if beacon.nonEmpty then
      println("beacon at " + beacon.get)
      println("freq = " + pointToFreq(beacon.get))
    else println("not found!")

end Day15

