package info.amrhassan.graphs.sparse

import info.amrhassan.graphs.{Graph, Edge}

case class SparseGraph[Vertex] private(isDirected: Boolean, vertices: Set[Vertex], edges: List[Edge[Vertex]],
                                       _edgesFrom: Map[Vertex, List[Edge[Vertex]]],
                                       _edgesTo: Map[Vertex, List[Edge[Vertex]]])
  extends Graph[Vertex] {

  override def connect(v1: Vertex, v2: Vertex): Graph[Vertex] = {
    val newEdge = Edge(v1, v2)
    if (isDirected) {
     SparseGraph(isDirected = true,
        vertices + v1 + v2,
        newEdge :: edges,
        _edgesFrom.updated(v1, newEdge :: _edgesFrom(v1)),
        _edgesTo.updated(v2, newEdge :: _edgesTo(v2))
      )
    } else {
      SparseGraph(isDirected = false,
        vertices + v1 + v2,
        newEdge :: edges,
        _edgesFrom.updated(v1, newEdge :: _edgesFrom(v1)).updated(v2, newEdge :: _edgesFrom(v2)),
        _edgesTo.updated(v2, newEdge :: _edgesTo(v2)).updated(v1, newEdge :: _edgesTo(v1))
      )
    }
  }

  override def edgesFrom(v: Vertex): Traversable[Edge[Vertex]] =
    _edgesFrom(v)


  override def edgesTo(v: Vertex): Traversable[Edge[Vertex]] =
    _edgesTo(v)
}

object SparseGraph {

  def emptyDirected[Vertex] =
    SparseGraph(isDirected = true,
      Set.empty[Vertex],
      List.empty[Edge[Vertex]],
      Map.empty.withDefaultValue(Nil),
      Map.empty.withDefaultValue(Nil))

  def emptyUndirected[Vertex] =
    SparseGraph(isDirected = false,
      Set.empty[Vertex],
      List.empty[Edge[Vertex]],
      Map.empty.withDefaultValue(Nil),
      Map.empty.withDefaultValue(Nil)
    )
}
