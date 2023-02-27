trait DayX(day: Int):
  
  private val filename = "day" + day + ".txt"
  val lineList: List[String] = FileFactory.toLineList(filename).getOrElse(List())
  
  def runner(): Unit

  def show(extra: String): Unit =
    println("")
    println("Day" + day + ": " + extra)
    if lineList.nonEmpty then runner()
    else println("empty file = " + filename)

  