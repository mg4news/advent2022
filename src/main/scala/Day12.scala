object Day12 extends DayX(12):

  private def canMove(from: Char, to: Char): Boolean =
    def helper(c: Char): Char = if c == 'S' then 'a' else if c == 'E' then 'z' else c
    if from == 'E' then false else helper(to).toInt - helper(from).toInt <= 1

  // all new possible neighbours
  // The history is the set of moves required to reach this point
  private def legalNeighbours(point: Point, grid: Grid[Char], length: Int): LazyList[(Point, Int)] =
    val res = grid.adjacent(point).filter(p => canMove(grid(point), grid(p))).map(p => (p, length+1))
    res.to(LazyList)

  private def newNeighbours(neighbours: LazyList[(Point, Int)], explored: Set[Point]): LazyList[(Point, Int)] =
    neighbours.filterNot(elem => explored.contains(elem._1))

  private def deDup(ll: LazyList[(Point, Int)]): LazyList[(Point, Int)] =
    if ll.length < 2 then ll else ll.toSet.to(LazyList)

  private def from(initial: LazyList[(Point, Int)], explored: Set[Point], grid: Grid[Char]): LazyList[(Point, Int)] =
    if initial.isEmpty then LazyList.empty
    else
      val more = deDup(for {
        (point, length) <- initial
        next <- newNeighbours(legalNeighbours(point, grid, length), explored)
      } yield next)
      initial ++ from(more, explored ++ more.map(_._1), grid)

  def isChar(p: Point, grid: Grid[Char], c: Char): Boolean = grid(p) == c

  override def runner(ls: List[String]): Unit =
    val lc = ls.map(s => s.toList)
    val grid = Grid[Char](ls.head.length, ls.length, 0, 0)
    grid.fill(ls.map(_.toList))
    if ls.length < 10 then grid.show()

    val start = grid.find('S').getOrElse(Point(0,0))
    val end = grid.find('E').getOrElse(Point(0,0))

    lazy val pathsFromStart: LazyList[(Point, Int)] = from(LazyList((start, 0)), Set(start), grid)
    lazy val pathsToEnd = pathsFromStart.filter(e => isChar(e._1, grid, 'E'))
    println("Part 1: path length = " + pathsToEnd.head._2)

    val la = grid.findAll('a')
    lazy val pathsFromA: LazyList[(Point, Int)] = from(
      la.map(p => (p, 0)).to(LazyList),
      la.toSet,
      grid)
    lazy val pathsAtoE = pathsFromA.filter(e => isChar(e._1, grid, 'E'))
    println("Part 2: path length = " + pathsAtoE.head._2)
  
end Day12

