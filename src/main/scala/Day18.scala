import scala.annotation.tailrec

object Day18 extends DayX(18):

  private def parseXYZ(s: String): (Int, Int, Int) =
    val split = s.split(",")
    assert(split.length == 3)
    (split(0).toInt, split(1).toInt, split(2).toInt)

  private def adjacent(x: Int, y: Int, z: Int): Set[(Int,Int,Int)] =
    Set((x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1))

  private val cubes = lineList.map(parseXYZ).toSet
  //private val testList = FileFactory.toLineList("day18test.txt").getOrElse(List())
  //private val cubes = testList.map(parseXYZ).toSet

  private def totalFaces(): Int =
    cubes.foldLeft(0) { case (total, (x,y,z)) =>
      val sa = adjacent(x,y,z)
      total + 6 - sa.count(cubes.contains)
    }

  override def runner(): Unit =
    // part 1
    val total = totalFaces()
    println("total = " + total)

end Day18

