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

  private[graphs] case class TraversalEntry(vertex: Vertex, leadingEdge: Option[Edge[Vertex]])

  /**
   * This function should accept the old queue as a first argument and the newly discovered items to queue as the
   * second argument.
   */
  private[graphs] type QueueCombiner = (List[TraversalEntry], List[TraversalEntry]) => List[TraversalEntry]

  private[graphs] case class History(explored: Set[Vertex],
                             private val doneExploringCallbacks: List[(Set[Vertex], () => Unit)]) {

    def has(v: Vertex): Boolean = explored contains v

    def +(entry: TraversalEntry): History = {

      val updatedExplored = explored + entry.vertex

      doneExploringCallbacks foreach { case (vertices, callback) =>
        if (vertices subsetOf updatedExplored)
          callback()
      }

      History(updatedExplored,
        doneExploringCallbacks filterNot {case (vertices, _) => vertices subsetOf updatedExplored})
    }

    /**
     * Registers a callback to be executed when done exploring this vertex.
     */
    def onDoneExploring(vertices: Set[Vertex], callback: () => Unit): History =
      History(explored, (vertices, callback) :: doneExploringCallbacks)
  }

  private object History {
    def apply(): History = History(Set.empty, List.empty)
    def apply(explored: Set[Vertex]): History = History(explored, List.empty)
  }

  private def traverse(start: Vertex,
                       onVisit: (TraversalEntry) => Unit,
                       combineQueued: QueueCombiner,
                       onFinishNeighbours: (Vertex) => Unit,
                       previousHistory: History): History = {

    def explore(entry: TraversalEntry, queued: List[TraversalEntry], history: History): History = {
      if (history has entry.vertex) {
        if (queued.isEmpty)
          history
        else
          explore(queued.head, queued.tail, history)
      } else {
        onVisit(entry)

        val outgoingEdges = edgesFrom(entry.vertex)

        val neighbours = (outgoingEdges map (_.otherVertex(entry.vertex))).toSet

        val newVertices = outgoingEdges map { edge =>
          TraversalEntry(edge.otherVertex(entry.vertex), Some(edge))
        }

        val updatedQueue = combineQueued(queued, newVertices.toList)

        val updatedHistory = history.onDoneExploring(neighbours, () => onFinishNeighbours(entry.vertex)) + entry

        if (updatedQueue.isEmpty) {
          updatedHistory
        } else {
          explore(updatedQueue.head, updatedQueue.tail, updatedHistory)
        }
      }
    }

    explore(TraversalEntry(start, None), List.empty, previousHistory)
  }

  /**
   * Performs Breadth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def bfs(start: Vertex, history: History = History())
         (onVisit: (TraversalEntry) => Unit)
         (onFinishNeighbours: (Vertex) => Unit): History =
    traverse(start, onVisit, (oldQueue, newQueue) => oldQueue ++ newQueue, onFinishNeighbours, history)

  /**
   * Performs Depth-first Search in the graph from the given vertex. Returns the set of explored vertices.
   */
  def dfs(start: Vertex, history: History = History())
         (onVisit: (TraversalEntry) => Unit)
         (onFinishNeighbours: (Vertex) => Unit): History =
    traverse(start, onVisit, (oldQueue, newQueue) => newQueue ++ oldQueue, onFinishNeighbours, history)

  /**
   * Computes and returns the minimum number of hops required to navigate from the first to the second vertex.
   */
  def shortestDistance(from: Vertex, to: Vertex): Int = {
    val distances = mutable.Map.empty[Vertex, Double]
    bfs(from) { case TraversalEntry(vertex, edgeOption) => edgeOption match {
      case None => distances(vertex) = 0
      case Some(edge) => distances(vertex) = distances(edge otherVertex vertex) + 1
    }
    }(_ => {})
    distances(to).toInt
  }

  lazy val connectedComponents: Set[Set[Vertex]] = {
    require(!isDirected)

    def loop(exploredSoFar: Set[Vertex], discoveredSoFar: Set[Set[Vertex]]): Set[Set[Vertex]] = {
      val unexplored = vertices -- exploredSoFar
      if (unexplored.isEmpty) {
        discoveredSoFar
      } else {
        val newConnectedComponent = bfs(unexplored.head)(DoNothing)(DoNothing).explored
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

    def order(history: History, currentOrder: List[Vertex]) : List[Vertex] = {
      val unexplored = vertices -- history.explored

      if (unexplored.isEmpty) {
        currentOrder
      } else {
        val newOrder = mutable.ListBuffer() ++ currentOrder
        val updatedHistory = dfs(unexplored.head, history)(DoNothing)(finishedVertex => newOrder.prepend(finishedVertex))
        order(updatedHistory, newOrder.toList)
      }
    }

    order(History(), Nil)
  }

  def areConnected(v1: Vertex, v2: Vertex): Boolean =
    bfs(v1)(DoNothing)(DoNothing).explored contains v2
}
