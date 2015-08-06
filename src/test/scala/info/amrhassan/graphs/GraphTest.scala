package info.amrhassan.graphs

import info.amrhassan.graphs.sparse.SparseGraph

class GraphTest extends org.scalatest.FunSuite {

  // These tests are not exhaustive, I know. They're just a simple sanity check.

  test("computing shortest distances") {

    var g: Graph[String] = SparseGraph.emptyUndirected
    g = g.connect("s", "a")
    g = g.connect("s", "b")
    g = g.connect("a", "c")
    g = g.connect("b", "c")
    g = g.connect("b", "d")
    g = g.connect("c", "d")
    g = g.connect("c", "e")
    g = g.connect("d", "e")

    assert(g.shortestDistance("s", "s") === 0)
    assert(g.shortestDistance("s", "a") === 1)
    assert(g.shortestDistance("s", "b") === 1)
    assert(g.shortestDistance("s", "c") === 2)
  }

  test("computing connected components") {

    var g: Graph[Int] = SparseGraph.emptyUndirected[Int]

    g = g.connect(1, 3)
    g = g.connect(1, 5)
    g = g.connect(3, 5)
    g = g.connect(7, 5)
    g = g.connect(9, 5)

    g = g.connect(2, 4)

    g = g.connect(6, 8)
    g = g.connect(6, 10)
    g = g.connect(8, 10)

    assert(g.connectedComponents === Set(
      Set(1, 3, 5, 7, 9),
      Set(2, 4),
      Set(6, 8, 10)
    ))
  }

  test("computing topological order") {

    var g: Graph[String] = SparseGraph.emptyDirected
    g = g.connect("s", "v")
    g = g.connect("s", "w")
    g = g.connect("s", "x")
    g = g.connect("x", "t")
    g = g.connect("v", "t")
    g = g.connect("w", "t")

    g = g.connect("m", "u")

    var order: List[String] = g.topologicallyOrdered.toList

    if (order.head == "m") {
      assert(order.tail.head === "u")
      order = order drop 2
    }

    assert(order.head === "s")
    assert(Set("v", "w", "x") contains order.tail.head)
    assert((order drop 4).head === "t")

    order = order drop 5

    if (order.nonEmpty) {
      assert(order === List("m", "u"))
    }
  }
}
