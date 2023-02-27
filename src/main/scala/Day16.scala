import scala.annotation.tailrec
import scala.collection.BitSet

object Day16 extends DayX(16):

  private val test: List[String] = List(
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
    "Valve HH has flow rate=22; tunnel leads to valve GG",
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
    "Valve JJ has flow rate=21; tunnel leads to valve II"  )

  private def stringToValve(s: String): Valve = s match
    case s"Valve $name has flow rate=$rate; tunnels lead to valves $leadTo" =>
      Valve(name, rate.toInt, leadTo.split(", ").toList)
    case s"Valve $name has flow rate=$rate; tunnel leads to valve $leadTo" =>
      Valve(name, rate.toInt, List(leadTo))

  private case class Valve(name: String, flowRate: Int, leadTo: List[String]):
    override def toString: String = name + ", rate=" + flowRate + ", leads to: " + leadTo

  //private val valveList = lineList.map(stringToValve)
  private val valveList = test.map(stringToValve)
  private val nameMap = valveList.zipWithIndex.map((v,i) => v.name -> i).toMap
  private val flowMap = valveList.zipWithIndex.map((v,i) => i -> v.flowRate).toMap
  private val moveMap = valveList.map(v => nameMap(v.name) -> v.leadTo.map(nameMap)).toMap

  // old shit
  private val valveMap = valveList.map(v => (v.name, v)).toMap
  private val totalFlow = valveList.foldLeft(0)((x, v) => x + v.flowRate)


  private case class Route(curr: Int, open: BitSet, stepValue: Int, accValue: Int):
    override def canEqual(that: Any): Boolean = that.isInstanceOf[Route]
    override def equals(that: Any): Boolean = that match
      case that: Route =>
        that.canEqual(this) &&
          this.curr == that.curr &&
          this.stepValue == that.stepValue &&
          this.accValue == that.accValue
      case _ => false
    override def hashCode(): Int = stepValue * accValue * curr
    def moveTo(next: Int): Route = Route(next, open, stepValue, accValue + stepValue)
    def openValve(flow: Int): Route = Route(curr, open + curr, stepValue + flow, accValue + stepValue)


  //private def nextRoutes(route: Route, totalFlow: Int): List[Route] =
  //  // don't waste time if all the valves are open, juste increment the accumulated flow
  //  if totalFlow == route.stepValue then List(Route(route.curr, route.open, route.stepValue, route.accValue + route.stepValue))
  //  val valve = valveMap(route.curr)
  //  val rs = valve.leadTo.map(route.moveTo)
  //  if valve.flowRate > 0 && !route.open.contains(route.curr) then
  //    val openValve = route.openValve(valve.flowRate)
  //    if openValve.stepValue == totalFlow then
  //      List(openValve)
  //    else
  //      openValve :: rs
  //  else
  //    rs

  private def reduceRoutes(routes: List[Route]): List[Route] =
    def getNextSimilar(r: Route, rs: List[Route]): List[Route] = rs match
      case h :: _ if (h.curr == r.curr) && (h.stepValue == r.stepValue) && (h.open == r.open) =>
        h :: getNextSimilar(r, rs.tail)
      case _ => Nil
  
    def reduceSimilar(rs: List[Route]): List[Route] = rs match
      case h :: _ =>
        val similar = getNextSimilar(h, rs)
        if similar.length > 1 then
          similar.maxBy(r => r.accValue) :: reduceSimilar(rs.drop(similar.length))
        else h :: reduceSimilar(rs.tail)
      case _ => Nil
  
    val nodup = routes.toSet.toList.sortBy(r => (r.curr, r.stepValue, r.accValue))
    reduceSimilar(nodup).sortBy(r => (r.stepValue, r.accValue))


  // recursive next
  // Take
  // - number of steps remaining
  // - a list of Route, each one is:
  //   - current valve
  //   - the list of actions to get there
  //   - the list of open valves for that action set
  //
  // For each Route
  // - get the next set of possible actions (move, open, etc)
  // - build N new lists (discarding the old one)
  // - de-duplicate to avoid accumulating too many lists
  //
  // Have a list of lists, flatten the results
  //@tailrec
  //private def routeFinder(lr: List[Route], count: Int, totalFlow: Int): List[Route] =
  //  if count <= 0 || lr.isEmpty then lr
  //  else
  //    println("Step = " + count)
  //    val routes = lr.flatMap(r => nextRoutes(vm, r, totalFlow))
  //    println("- routes = " + routes.length)
  //    val reduced  = reduceRoutes(routes).sortBy(r => r.accValue).reverse
  //    println("- reduced = " + reduced.length)
  //    if reduced.head.stepValue == totalFlow then
  //      println("complete at " + count)
  //      List(Route(reduced.head.curr, "", totalFlow, reduced.head.accValue + (count-1) * totalFlow))
  //    else
  //    routeFinder(vm, reduced, count-1, totalFlow)

  // Make 4 steps
  //private def routeWrapper(routes: List[Route], totalCount: Int, totalFlow: Int): List[Route] =
  //  val increment = 5

    //def helper(lr: List[Route], count: Int): List[Route] =
    //  if count == 0 then lr
    //  else
    //    val inc = increment.min(count)
    //    val nr = routeFinder(lr, inc, totalFlow)
    //    val newCount = count - inc
    //    println("= Analyse at " + newCount)
    //    if newCount > 0 && nr.length > 1 then
    //      val minAccValue  = (nr.head.accValue * (totalCount - newCount)) / totalCount
    //      val minStepValue = (nr.head.stepValue * (totalCount - newCount)) / totalCount
    //      println("= max acc = " + nr.head.accValue + ", max step = " + nr.head.stepValue)
    //      println("= min acc = " + minAccValue + ", min step = " + minStepValue)
    //      println("= reduce from " + nr.length + " to " + nr.count(r => (r.accValue > minAccValue && r.stepValue > minStepValue)))
    //      helper(nr.filter(r => (r.accValue > minAccValue && r.stepValue > minStepValue)), newCount)
    //    else
    //      nr

    //helper(routes, totalCount)

  override def runner(): Unit =
    println("Total possible flow = " + totalFlow)

    // map of all valves that have a flow rate of zero, i.e. no point opening, so consider them open
    //val open = valveList.filter(_.flowRate <= 0).map(_.name).sorted
    //val start = Route("AA", open.mkString, 0, 0)
    //val routes = routeWrapper(valveMap, List(start), 30, totalFlow)
    //println("possible routes = " + routes.length)
    //println("max = " + routes.maxBy(r => r.accValue))


end Day16

