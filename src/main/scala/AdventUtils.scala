// Helper functions
object AdventUtils:
  
  // Returns true if the string contains (i.e. substring) any of the strings from the set
  // Note that this does not just look at words, it looks at full case sensitive substrings
  private def fltAnySubStrings(ss: Set[String], s: String): Boolean = ss.exists(s.contains)

  /**
   * Trims lines in a List[Lines/string].
   * The trim string is used to split each string in the map into "trim, remainder". If there is a "remainder", 
   * i.e. a substring after the trim string, then that remainder becomes part of a new map. If there is no remainder, 
   * then the trim string did not cause a split, and we discard the entire string.
   * @param ts Trim string
   * @param ls List of strings 
   * @return  A new file map
   */
  def trim(ts: String, ls: List[String]) : List[String] = 
    for l <- ls
      remainder = l.split(ts)
      if remainder.length > 1
    yield remainder(1)

  /**
   * Removes all empty strings (lines) from a list of strings
   * @param ls List of strings, may have empty lines 
   * @return List of strings with no empty lines
   */
  def toNonEmpty(ls: List[String]): List[String] = 
    for l <- ls if l.nonEmpty yield l

  // Returns a new FileMap including ONLY lines containing ANY substring in the set
  // @param ss  Set of strings (may contain multiple words)
  // @result     A new file map
  def hasAnySubStrings(ss: Set[String], ls: List[String]) : List[String] =
    for
      l <- ls
      if fltAnySubStrings(ss, l)
    yield l

  // Returns a new FileMap including ONLY lines containing NO substring in the set
  // @param ss  Set of strings (may contain multiple words)
  // @result     A new file map
  def hasNoSubStrings(ss: Set[String], ls: List[String]) : List[String] =
    for
      l <- ls
      if !fltAnySubStrings(ss, l)
    yield l

  def splitByEmptyString(ls: List[String]) : List[List[String]] =
    ls.span(s => s.nonEmpty) match
      case (h, _::t) => h :: splitByEmptyString(t)
      case (h, _) => List(h)
      
  // Splits a list of strings bt either:
  // - blank string (if no string or None is provided)
  // - by a matching string
  // DROPS the matching string 
  def splitByMatching(s: String, ls: List[String]) : List[List[String]] = 
    ls.span(l => l == s) match
        case (h, _ :: t) => h :: splitByMatching(s, t)
        case (h, _) => List(h)

  /**
   * Converts a list of integer lists into a list of the sums of each list
   * @param ll list of integer lists
   * @return list of integers, each integer is the sum of one list
   */
  def listOfSums(ll: List[List[Int]]): List[Int] = ll.map(l => l.sum)

  def extendAt[A](ll: List[List[A]], idx: Int, la: List[A]): List[List[A]] =
    val t = ll.drop(idx)
    ll.take(idx) ::: (t.head ::: la) :: t.tail

  /**
   * Draws a "grid" i.e. a 2D array
   * @param grid: A 2D array
   * @param dense: If true no spaces, else one space
   * @tparam A: The grid element type
   */
  def drawGrid[A](grid: Array[Array[A]], dense: Boolean = true): Unit =
    for (y <- grid(0).indices)
      for (x <- grid.indices)
        print(grid(x)(y))
        if (!dense) print(" ")
      println
