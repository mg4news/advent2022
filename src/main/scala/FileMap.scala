case class FileMap(lm: IndexedSeq[(Int,String)]):

  // Default number of lines to print in a snapshot
  private val SNAPSHOT_MAX_LINES: Int = 5

  // Returns true if the string contains (i.e. substring) any of the strings from the set
  // Note that this does not just look at words, it looks at full case sensitive substrings
  private def fltAnySubStrings(ss: Set[String], s: String): Boolean = ss.exists(s.contains)

  // Number of pairs
  def size: Int = lm.length

  // Dump information about the map, i.e. size, a few of the lines, etc.
  def snapshot(lines: Int = SNAPSHOT_MAX_LINES): Unit =
    println("Map snapshot:")
    println("- line count = " + size)
    println("- dumping first " + math.min(size, lines) + " lines:")
    for (i <- 0 until math.min(size, lines)) println("  " + lm(i))

  // Removes all empty strings
  def toNonEmptyMap: FileMap = FileMap(
    for pair <- lm if pair._2.nonEmpty yield pair
  )

  // Reindex. Sometimes after removing lines we dont hare about the original index
  def reindex: FileMap = FileMap(
    for i <- lm.indices
        l = lm(i)
    yield (i+1, l._2)
  )

  // Returns a new FileMap including ONLY lines containing ANY substring in the set
  // @param ss  Set of strings (may contain multiple words)
  // @result     A new file map
  def hasAnySubStrings(ss: Set[String]): FileMap = FileMap(
    for
      pair <- lm
      if fltAnySubStrings(ss, pair._2)
    yield pair)

  // Returns a new FileMap including ONLY lines containing NO substring in the set
  // @param ss  Set of strings (may contain multiple words)
  // @result     A new file map
  def hasNoSubStrings(ss: Set[String]): FileMap = FileMap(
    for
      pair <- lm
      if !fltAnySubStrings(ss, pair._2)
    yield pair)

  // Trims lines in a FileMap.
  // The trim string is used to split each string in the map into "trim, remainder". If there is a "remainder",
  // i.e. a substring after the trim string, then that remainder becomes part of a new map. If there is no remainder,
  // then the trim string did not cause a split, and we discard the entire string.
  // @param t Trim string
  // @result  A new file map
  def trim(ts: String): FileMap = FileMap(
    for
      pair <- lm
      remainder = pair._2.split(ts)
      if remainder.length > 1
    yield (pair._1, remainder(1)))

