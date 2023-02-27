object Day10 extends DayX(10) {

  private def addxList(ls: List[String]): List[Int] = ls.flatMap {
        case "noop" => List(0)
        case s"addx $amount" => List(0, amount.toInt)
      }

  private def execute(curr: Int, rl: List[Int]): List[Int] =
    rl.scanLeft(curr)(_ + _)

  private def signalStrength(measure: List[Int], reg: List[Int]): List[Int] =
    val totals = for i <- measure.indices yield measure(i) * reg(measure(i)-1)
    totals.toList

  // if any part if sprite (3 wide) is visible
  private def drawCrt(reg: List[Int], width: Int): List[String] =
    val lc = for (r,pixel) <- reg.zipWithIndex yield
      if (r - (pixel % width)).abs <= 1 then '#' else '.'
    lc.grouped(width).map(_.mkString).toList

  override def runner(): Unit =
    println("instructions: " + lineList.length)
    val reg = execute(1, addxList(lineList))
    println("result size = " + reg.length)

    //println("reg(20)  = " + reg(19)*20)
    //println("reg(60)  = " + reg(59)*60)
    //println("reg(100) = " + reg(99)*100)
    //println("reg(140) = " + reg(139)*140)
    //println("reg(180) = " + reg(179)*180)
    //println("reg(220) = " + reg(219)*220)
    val clks = List(20,60,100,140,180,220)
    val strength = signalStrength(clks, reg)
    println("signal strength = " + strength +" = " + strength.sum)
    println("CRT = ")
    drawCrt(reg,40).foreach(println)
}
