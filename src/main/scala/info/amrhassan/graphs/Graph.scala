package info.amrhassan.graphs

import scala.collection.immutable.Queue
import scala.collection.mutable

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
  def bfs(start: Vertex)(onVisit: (Vertex, Option[Edge[Vertex]]) => Unit): Set[Vertex] = {

    // The queue entry is made up of the vertex being explored and optionally the edge leading there. Only
    // the source vertex would have no edge leading to it in the traversal.
    type queueEntry = (Vertex, Option[Edge[Vertex]])

    def bfs(explored: Set[Vertex], queue: Queue[queueEntry]): Set[Vertex] = {
      if (queue.isEmpty) {
        explored
      } else {
        val ((vertex, sourceEdge), rest) = queue.dequeue
        if (explored contains vertex)
          bfs(explored, rest)
        else  {
          onVisit(vertex, sourceEdge)
          val connectedVertices = edgesFrom(vertex) flatMap { edge =>
            (edge.vertexSet - vertex).headOption filterNot explored.contains map((_, Some(edge)))
          }
          bfs(explored + vertex, rest ++ connectedVertices)
        }
      }
    }

    bfs(Set.empty, Queue((start, None)))
  }

  /**
   * Performs Depth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def dfs(start: Vertex)(onVisit: (Vertex) => Unit)(onFinishingNeighbours: (Vertex) => Unit): Set[Vertex] = {

    def dfs(explored: Set[Vertex], vertex: Vertex): Set[Vertex] = {
      if (explored contains vertex) {
        explored
      } else {
        onVisit(vertex)
        var exploredInProgress = explored + vertex
        val connectedVertices = edgesFrom(vertex) flatMap { edge =>
          (edge.vertexSet - vertex).headOption filterNot explored.contains
        }
        connectedVertices foreach { newVertex =>
          exploredInProgress ++= dfs(exploredInProgress, newVertex)
        }
        onFinishingNeighbours(vertex)
        exploredInProgress
      }
    }

    dfs(Set.empty, start)
  }

  /**
   * Computes and returns the minimum number of hops required to navigate from the first to the second vertex.
   */
  def shortestDistance(from: Vertex, to: Vertex): Int = {
    val distances = mutable.Map.empty[Vertex, Double].withDefaultValue(Double.PositiveInfinity)
    bfs(from) { (vertex, edgeOption) =>
      if (edgeOption.isEmpty)
        distances(vertex) = 0.0
      else
        distances(vertex) = distances(edgeOption.get.otherVertex(vertex).get) + 1
    }
    distances(to).toInt
  }

  def connectedComponents: Set[Set[Vertex]] = {
    require(!isDirected)

    def connectedComponents(exploredSoFar: Set[Vertex], discoveredSoFar: Set[Set[Vertex]]): Set[Set[Vertex]] = {
      val unexplored = vertices -- exploredSoFar
      if (unexplored.isEmpty) {
        discoveredSoFar
      } else {
        val newConnectedComponent = bfs(unexplored.head){case (_, _) =>}
        connectedComponents(exploredSoFar ++ newConnectedComponent, discoveredSoFar + newConnectedComponent)
      }
    }

    connectedComponents(Set.empty, Set.empty)
  }

  /**
   * Returns a topologically-ordered traversable of this acyclic directed graph.
   */
  lazy val topologicallyOrdered: Traversable[Vertex] = {
    require(isDirected)
    var orderedVertices: List[Vertex] = Nil
    var explored = Set.empty[Vertex]

    while (explored != vertices) {
      explored ++= dfs(vertices.head)(_ => {})(finishedVertex => orderedVertices = finishedVertex :: orderedVertices)
    }
    orderedVertices
  }
}
