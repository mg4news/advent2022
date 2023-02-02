import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Class to abstract a 2D grid (i.e. an Array(Array(T))
 */
case class Grid[T: ClassTag](width: Int, height: Int, xoff: Int, yoff: Int, fill: Option[T] = None) {

  override def toString: String = "grid(w=" + width + ",h=" + height + ") xoff=" + xoff + ", yoff=" + yoff

  val grid: Array[Array[T]] = fill match
    case None => Array.ofDim[T](width,height)
    case Some(v) => Array.fill[T](width,height)(v)

  /**
   * Updates the grid accounting for the offsets
   */
  def update(x: Int, y: Int, v: T): Unit = grid(x-xoff)(y-yoff) = v
  def apply(x: Int, y: Int): T = grid(x-xoff)(y-yoff)

  /**
   * index at a Point, accounting for offsets
   */
  def update(p: Point, v: T): Unit = grid(p.x-xoff)(p.y-yoff) = v
  def apply(p: Point): T = grid(p.x-xoff)(p.y-yoff)

  def contains(p: Point): Boolean =
    (p.x >= xoff) && (p.x < width - xoff) && (p.y >= yoff) && (p.y < height - yoff)

  def contains(x: Int, y: Int): Boolean =
    (x >= xoff) && (x < width - xoff) && (y >= yoff) && (y < height - yoff)

  def adjacent(p: Point): List[Point] =
    p.adjacent().filter(p => this.contains(p))

  /**
   * show (print) the grid
   * @param dense: if false insert a space between each element
   */
  def show(dense: Boolean = false): Unit =
    for (y <- grid(0).indices)
      for (x <- grid.indices)
        print(grid(x)(y))
        if (!dense) print(" ")
      println

  def find(elem: T): Option[Point] =
    @tailrec
    def helper(ls: List[(Int,Int)]): Option[Point] = ls match
      case h::_ => if grid(h._1)(h._2) == elem then Some(Point(h._1, h._2)) else helper(ls.tail)
      case _ => None
    val coords = (for {
      x <- grid.indices
      y <- grid(0).indices
    } yield (x,y)).toList
    helper(coords)
    
  def findAll(elem: T): List[Point] =
    @tailrec
    def helper(ls: List[(Int,Int)], lp:  List[Point] = List.empty): List[Point] = ls match
      case h::_ => 
        if grid(h._1)(h._2) == elem then 
          helper(ls.tail, Point(h._1,h._2) :: lp)
        else helper(ls.tail, lp)  
      case _    => lp
    val coords = (for {
      x <- grid.indices
      y <- grid(0).indices
    } yield (x, y)).toList
    helper(coords)

  def fill(ls: List[List[T]]): Unit =
    var y = 0
    for (row <- ls)
      val cs = row.toIndexedSeq
      for (x <- cs.indices)
        grid(x)(y) = cs(x)
      y = y + 1

}

object Grid {
  /**
   * Build a grid from a list of points
   */
  def apply[T:ClassTag](lp: List[Point], fill: Option[T]): Grid[T] =
    println("points list: " + lp)
    val (minp, maxp) = Point.getLimits(lp)
    println("min point: " + minp)
    println("max point: " + maxp)
    Grid[T](maxp.x - minp.x + 1, maxp.y - minp.y + 1, minp.x, minp.y, fill)

  /**
   * Build a grid from a min and max points
   */
  def apply[T:ClassTag](minp: Point, maxp: Point, fill: Option[T]): Grid[T] =
    Grid[T](maxp.x - minp.x + 1, maxp.y - minp.y + 1, minp.x, minp.y, fill)
}
