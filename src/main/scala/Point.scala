import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  // print me
  override def toString: String = "p(" + x + "," + y + ")"
  
  enum Dir {
    case Up, Down, Left, Right
  }

  /**
   * subtract a point from me (this - p)
   * @param p: point to subtract
   * @return result as a point
   */
  def sub(p: Point): Point = Point(x - p.x, y - p.y)

  /**
   * Add a point to me
   * @param p: point to add
   * @return result as a point
   */
  def add(p: Point): Point = Point(x+p.x, y+p.y)

  /**
   * Manhattan (linear) distance between two points
   * @param p: other point
   * @return linear distance
   */
  def distManhattan(p: Point): Int = (x-p.x).abs + (y-p.y).abs

  /**
   * Increment in a specified direction
   * @param dir: direction
   * @return next point (no limit testing)
   */
  def increment(dir: Dir): Point = dir match
    case Dir.Up => Point(x,y-1)
    case Dir.Down => Point(x,y-1)
    case Dir.Left => Point(x-1,y)
    case Dir.Right => Point(x+1,y)

  def adjacent(): List[Point] =
    List(Point(x,y-1), Point(x,y+1), Point(x-1,y), Point(x+1,y))
}

object Point {
  def getLimits(points: List[Point]): (Point, Point) =
    @tailrec
    def helper(lp: List[Point], xmin: Int, ymin:Int, xmax: Int, ymax: Int): (Point, Point) = lp match
      case h::_ => helper(lp.tail, h.x.min(xmin), h.y.min(ymin), h.x.max(xmax), h.y.max(ymax))
      case Nil => (Point(xmin,ymin), Point(xmax,ymax))
    if (points.isEmpty) (Point(0,0), Point(0,0)) else 
      val h = points.head
      helper(points, h.x, h.y, h.x, h.y)
}