import scala.annotation.tailrec

object Day8 extends DayX(8) {

  private sealed trait Dir
  private case object Up extends Dir
  private case object Down extends Dir
  private case object Left extends Dir
  private case object Right extends Dir

  private def nextTree(x: Int, y: Int, d: Dir, li: List[List[Int]]): Option[(Int,Int)] = d match
    case Up => if (y-1 >= 0) Some((x,y-1)) else None
    case Down => if (y+1 >= li.length) None else Some((x,y+1))
    case Left => if (x-1 >= 0) Some((x-1,y)) else None
    case Right => if (x+1 >= li.head.length) None else Some((x+1,y))

  private def canSee(x: Int, y: Int, d: Dir, li:List[List[Int]]): (Boolean, Int) =
    @tailrec
    def helper(xn: Int, yn: Int, acc:Int = 0): (Boolean,Int) = nextTree(xn,yn,d,li) match
      case None =>
        val adj = if (acc == 0) 1 else 0
        (true,acc+adj)
      case Some(xt,yt) => if (li(yt)(xt) >= li(y)(x)) (false,acc+1) else helper(xt,yt,acc+1)
    helper(x,y)

  private def treeVisible(x: Int, y: Int, li:List[List[Int]]): Boolean =
    canSee(x,y,Up,li)._1 || canSee(x,y,Down,li)._1 || canSee(x,y,Left,li)._1 || canSee(x,y,Right,li)._1

  private def scenicScore(x: Int, y: Int, li: List[List[Int]]): Int =
    canSee(x, y, Up, li)._2 * canSee(x, y, Down, li)._2 * canSee(x, y, Left, li)._2 * canSee(x, y, Right, li)._2

  private def visible(li:List[List[Int]]): Int =
    val w = li.head.length
    val h = li.length
    var v = 0
    for (x <- 1 until w-1) {
      for (y <- 1 until h-1) {
        if treeVisible(x,y,li) then v = v + 1
      }
    }
    v + 2*w + 2*h - 4 // add in the edges, don't multi-count the corners

  private def maxScenicScore(li: List[List[Int]]): Int =
    val w = li.head.length
    val h = li.length
    var m = 0
    for (x <- 1 until w - 1) {
      for (y <- 1 until h - 1) {
        val sc = scenicScore(x,y,li)
        m = m.max (sc)
      }
    }
    m

  override def runner(ls: List[String]): Unit =
    val li = ls.map(_.toList.map(c => c.toInt - '0'.toInt))
    //val li = List(
    //  List(3,0,3,7,3),
    //  List(2,5,5,1,2),
    //  List(6,5,3,3,2),
    //  List(3,3,5,4,9),
    //  List(3,5,3,9,0))
    println("rows = " + li.length)
    println("cols = " + li.head.length)
    println("number of trees = " + li.length * li.head.length)
    println("number visible  = " + visible(li))
    println("max scenic score = " + maxScenicScore(li))
}
