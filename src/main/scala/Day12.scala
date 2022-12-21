object Day12 extends DayX(12) {

  case class Coord(x: Int, y: Int) {
    override def toString: String = "(" + x + "," + y + ")"
  }

  def findC(c: Char, lc: List[List[Char]]): Coord =
    val res = for {
      x <- lc.head.indices
      y <- lc.indices
      if c == lc(y)(x)
    } yield Coord(x,y)
    res.head

  // "S" becomes "a"
  // Can step up 1, same, or down many
  def canMove(f:Coord, t:Coord, lc: List[List[Char]]): Boolean =
    val from = if (lc(f.y)(f.x) == 'S') 'a' else lc(f.y)(f.x)
    val to = lc(t.y)(t.x)
    val diff = to.toInt - from.toInt
    if (lc(f.y)(f.x) == 'E' || diff <= 0 || diff == 1)
      //println(" > " + f + "=" + lc(f.y)(f.x) + " to " + t + "=" + lc(t.y)(t.x))
      true
    else false

  // all legal neighbours
  def legalNext(coord:Coord, lc: List[List[Char]]): List[Coord] =
    val x = coord.x
    val y = coord.y
    List(Coord(x+1,y), Coord(x-1,y), Coord(x,y+1), Coord(x,y-1)).
      filter(c => c.x >= 0 && c.y >= 0).
      filter(c => c.x < lc.head.length && c.y < lc.length).
      filter(c => canMove(coord, c, lc))
    
  // Takes all the legal neighbours, for each generate a lazylist entry of the new coordinate and the path/history to get theie   
  def nextWithHistory(lc: List[List[Char]] ,c: Coord, history: List[Coord]): LazyList[(Coord, List[Coord])] =
    legalNext(c, lc).map(elem => (elem, c::history)).to(LazyList)

  // all NEW next, with history
  def newNext(neighbours: LazyList[(Coord, List[Coord])], explored: Set[Coord]): LazyList[(Coord, List[Coord])] =
    neighbours.filterNot(elem => explored.contains(elem._1))

  // Stream of all possible paths that can be followed, starting at the head if the "initial" LazyList
  def from(lc: List[List[Char]],
           initial: LazyList[(Coord, List[Coord])],
           explored: Set[Coord]): LazyList[(Coord, List[Coord])] =
    if (initial.isEmpty) LazyList.empty
    else {
      val more = for {
        (coord, moves) <- initial
        next <- newNext(nextWithHistory(lc, coord, moves), explored)
      } yield next
      initial ++ from(lc, more, explored ++ more.map(_._1))
    }

  def isEnd(c: Coord, lc: List[List[Char]]): Boolean =
    if (lc(c.y)(c.x) == 'E') true else false

  override def runner(ls: List[String]): Unit =
    val lc = ls.map(s => s.toList)
    val start = findC('S', lc)
    val end = findC('E', lc)
    println("start = " + start)
    println("end = " + end)

    lazy val pathsFromStart: LazyList[(Coord, List[Coord])] = from(lc, LazyList((start,List.empty[Coord])), Set(start))
    lazy val pathsToEnd: LazyList[(Coord, List[Coord])] = pathsFromStart.filter(elem => isEnd(elem._1, lc))
    val paths = pathsToEnd.map(elem => elem._2.length).sorted.reverse
    println("length = " + paths.head)



}
