trait DayX(day: Int):
  def runner(ls: List[String]): Unit

  def show(extra: String): Unit =
    println("")
    println("Day" + day + ": " + extra)
    val filename = "day" + day + ".txt"
    val ol = FileFactory.toLineList(filename)
    ol match
      case None => println("empty file = " + filename)
      case Some(ls) => runner(ls)
