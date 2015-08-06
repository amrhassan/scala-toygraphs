package info.amrhassan.graphs

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

  def edgesTo(v: Vertex): Traversable[Edge[Vertex]]

  def neighboursOf(v: Vertex): Traversable[Vertex] =
    edgesFrom(v) map (edge => edge.otherVertex(v))

  def reverseNeighboursOf(v: Vertex): Traversable[Vertex] =
    edgesTo(v) map (edge => edge.otherVertex(v))

  private[graphs] case class TraversalEntry(vertex: Vertex, leadingEdge: Option[Edge[Vertex]])

  /**
   * Performs Breadth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def bfs(start: Vertex, history: Set[Vertex] = Set.empty)(onVisit: (TraversalEntry) => Unit): Set[Vertex] = {

    def explore(entry: TraversalEntry, queued: List[TraversalEntry], history: Set[Vertex]): Set[Vertex] = {
      if (history contains entry.vertex) {
        if (queued.isEmpty)
          history
        else
          explore(queued.head, queued.tail, history)
      } else {
        onVisit(entry)
        val updatedHistory = history + entry.vertex

        val outgoingEdges = edgesFrom(entry.vertex)

        val newEntries = outgoingEdges map { edge =>
          TraversalEntry(edge.otherVertex(entry.vertex), Some(edge))
        }

        val updatedQueue = queued ++ newEntries.toList

        if (updatedQueue.isEmpty) {
          updatedHistory
        } else {
          explore(updatedQueue.head, updatedQueue.tail, updatedHistory)
        }
      }
    }

    explore(TraversalEntry(start, None), List.empty, history)
  }


  /**
   * Performs Depth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def dfs(start: Vertex, history: Set[Vertex] = Set.empty, reverse: Boolean = false)
         (onVisit: (Vertex) => Unit)
         (onFinishNeighbours: (Vertex) => Unit): Set[Vertex] = {

    val mutableHistory = mutable.Set.empty[Vertex] ++ history

    def uglyDFS(v: Vertex): Unit = {
      if (mutableHistory contains v)
        return

      mutableHistory += v
      onVisit(v)

      val neighbours = if (reverse) reverseNeighboursOf(v) else neighboursOf(v)
      neighbours foreach uglyDFS

      onFinishNeighbours(v)
    }

    uglyDFS(start)
    mutableHistory.toSet
  }

  /**
   * Computes and returns the minimum number of hops required to navigate from the first to the second vertex.
   */
  def shortestDistance(from: Vertex, to: Vertex): Int = {
    val distances = mutable.Map.empty[Vertex, Double]
    bfs(from) { case TraversalEntry(vertex, edgeOption) => edgeOption match {
      case None => distances(vertex) = 0
      case Some(edge) => distances(vertex) = distances(edge otherVertex vertex) + 1
    }
    }
    distances(to).toInt
  }

  lazy val connectedComponents: Set[Set[Vertex]] = {
    require(!isDirected)

    def loop(exploredSoFar: Set[Vertex], discoveredSoFar: Set[Set[Vertex]]): Set[Set[Vertex]] = {
      val unexplored = vertices -- exploredSoFar
      if (unexplored.isEmpty) {
        discoveredSoFar
      } else {
        val newConnectedComponent = bfs(unexplored.head)(DoNothing)
        loop(exploredSoFar ++ newConnectedComponent, discoveredSoFar + newConnectedComponent)
      }
    }

    loop(Set.empty, Set.empty)
  }

  /**
   * Returns a topologically-ordered traversable of this acyclic directed graph.
   */
  lazy val topologicallyOrdered: Traversable[Vertex] = {
    require(isDirected)

    val ordered = mutable.ListBuffer.empty[Vertex]
    var history = Set.empty[Vertex]

    while ((vertices -- history).nonEmpty) {
      history = dfs((vertices -- history).head, history)(DoNothing) { finishedVertex =>
        ordered.prepend(finishedVertex)
      }
    }

    ordered.toSeq
  }

  def areConnected(v1: Vertex, v2: Vertex): Boolean =
    bfs(v1)(DoNothing) contains v2
}
