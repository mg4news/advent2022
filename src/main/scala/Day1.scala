object Day1 extends DayX(1):

  override def runner(ls: List[String]): Unit =
    // list of line lists, split at empty string
    // convert to list of list of ints
    // sum each list, get a list of sums
    // order the list be descending
    // - show head = max
    // - show sum of first 3
    val lls = ListHelpers.splitByEmptyString(ls)
    val lli = lls.map(l => l.map(s => s.toInt))
    val sums = ListHelpers.listOfSums(lli)
    val ordered = sums.sorted(Ordering.Int.reverse)
    println("max = " + ordered.head)
    println("sum of 3 = " + ordered.take(3).sum)

