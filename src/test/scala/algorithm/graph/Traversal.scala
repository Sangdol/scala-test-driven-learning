package algorithm.graph

object Traversal {
  def dfs[V](
      start: V,
      graph: Graph[V],
      f: V => Unit,
      visited: Set[V] = Set[V]()
  ): Set[V] = {
    if (visited.contains(start)) return visited

    f(start)
    graph.neighbours(start).foldLeft(visited + start) { (allVisited, v) =>
      dfs(v, graph, f, allVisited)
    }
  }

  def iterativeDfs[V](start: V, graph: Graph[V], f: V => Unit): Unit = {
    LazyList
      .iterate((List(start), Set[V](start))) {
        case (stack, visited) =>
          val node = stack.head
          val newStack =
            graph.neighbours(node).filterNot(visited.contains) ++ stack.tail
          val newVisited = graph.neighbours(node).toSet ++ visited
          (newStack, newVisited)
      }
      .takeWhile(_._1.nonEmpty)
      .foreach(a => f(a._1.head))
//      .foreach(f(_._1.head)) // why isn't this possible?
  }
}
