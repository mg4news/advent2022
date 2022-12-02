// Helper functions for lists of strings 
object StringListHelper:

  // Returns true if the string contains (i.e. substring) any of the strings from the set
  // Note that this does not just look at words, it looks at full case sensitive substrings
  private def fltAnySubStrings(ss: Set[String], s: String): Boolean = ss.exists(s.contains)

  // Trims lines in a List[Lines/string].
  // The trim string is used to split each string in the map into "trim, remainder". If there is a "remainder", 
  // i.e. a substring after the trim string, then that remainder becomes part of a new map. If there is no remainder, 
  // then the trim string did not cause a split, and we discard the entire string.
  // @param ts Trim string
  // @param ls List of strings 
  // @result  A new file map
  def trim(ts: String, ls: List[String]) : List[String] = 
    for l <- ls
      remainder = l.split(ts)
      if remainder.length > 1
    yield remainder(1)

  // Removes all empty strings
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


