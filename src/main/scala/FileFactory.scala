import scala.io.Source

object FileFactory:

  // Simple line list
  def toLineList(filename: String): Option[List[String]] =
    val l = Source.fromResource(filename).getLines().toList
    if (l.isEmpty)
      None
    else
      Some(l)

  // Converts a sequence of strings (lines) to a FileMap
  def toLineMap(filename: String): Option[FileMap] = toLineList(filename) match
    case None => None
    case Some(ll) => Some(FileMap(
      for i <- ll.indices 
          s = ll(i)
      yield (i + 1, s)
    ))
