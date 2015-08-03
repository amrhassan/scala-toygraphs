package info.amrhassan.graphs

import scala.collection.immutable.{Stack, Queue}

/**
 * An immutable persistent Graph.
 */
trait Graph[Vertex] {

  def isDirected: Boolean

  /**
   * Returns a new instance where v1 and v2 are connected.
   */
  def connect(v1: Vertex, v2: Vertex): Graph[Vertex]

  /**
   * The vertices contained in this graph.
   */
  def vertices: Set[Vertex]

  /**
   * The edges making up this graph.
   */
  def edges: Traversable[Edge[Vertex]]

  /**
   * Edges going out of the specified vertex.
   */
  def edgesFrom(v: Vertex): Traversable[Edge[Vertex]]


  /**
   * Performs Breadth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def bfs(start: Vertex)(callback: (Vertex) => Unit): Set[Vertex] = {

    def bfs(explored: Set[Vertex], queue: Queue[Vertex]): Set[Vertex] = {
      if (queue.isEmpty) {
        explored
      } else {
        val (head, rest) = queue.dequeue
        if (explored contains head)
          bfs(explored, rest)
        else  {
          callback(head)
          val connectedVertices = edgesFrom(head) flatMap { edge =>
            (edge.vertexSet - head).headOption filterNot explored.contains
          }
          bfs(explored + head, rest ++ connectedVertices)
        }
      }
    }

    bfs(Set.empty, Queue(start))
  }

  /**
   * Performs Depth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def dfs(start: Vertex)(callback: (Vertex) => Unit): Set[Vertex] = {

    def dfs(explored: Set[Vertex], queue: List[Vertex]): Set[Vertex] = {
      if (queue.isEmpty) {
        explored
      } else {
        val vertex :: rest = queue
        if (explored contains vertex) {
          dfs(explored, rest)
        } else {
          callback(vertex)
          val connectedVertices = edgesFrom(vertex) flatMap { edge =>
            (edge.vertexSet - vertex).headOption filterNot explored.contains
          }
          dfs(explored + vertex, connectedVertices.toList ::: rest)
        }
      }
    }
    dfs(Set.empty, List(start))
  }
}
