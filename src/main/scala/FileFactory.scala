import scala.io.Source

object FileFactory:

  /**
   * Generates an Option of a List of strings, each string is a line in the file
   * Includes empty lines as an empty string
   * @param filename: The file (relative to the resources directory)
   * @return option of line lists
   */
  def toLineList(filename: String): Option[List[String]] =
    val l = Source.fromResource(filename).getLines().toList
    if (l.isEmpty)
      None
    else
      Some(l)

  /**
   * Converts a text file to a (Option of) FileMap
   * @param filename The file (relative to the resources directory)
   * @return option of List[FileMap]
   */
  def toLineMap(filename: String): Option[FileMap] = toLineList(filename) match
    case None => None
    case Some(ll) => Some(FileMap(
      for i <- ll.indices 
          s = ll(i)
      yield (i + 1, s)
    ))
