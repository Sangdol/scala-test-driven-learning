package algorithm.graph

import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite {

  test("DFS test") {
    val graph = Graph[String]()
      .addEdge("a", "b")
      .addEdge("a", "c")
      .addEdge("b", "d")

    Traversal.dfs("a", graph, println)
    Traversal.iterativeDfs("a", graph, println)
  }
}
