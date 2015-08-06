package info.amrhassan.graphs.sparse

import info.amrhassan.graphs.{Graph, Edge}

case class SparseGraph[Vertex] private(isDirected: Boolean, vertices: Set[Vertex], edges: Traversable[Edge[Vertex]])
  extends Graph[Vertex] {

  override def connect(v1: Vertex, v2: Vertex): Graph[Vertex] =
    SparseGraph(isDirected, vertices + v1 + v2, edges ++ Seq(Edge(v1, v2)))

  // TODO: Should execute in constant time
  override def edgesFrom(v: Vertex): Traversable[Edge[Vertex]] =
    if (isDirected)
      edges filter(e => e.from == v)
    else
      edges filter(e => e.vertexSet contains v)
}

object SparseGraph {

  def emptyDirected[Vertex] = SparseGraph(isDirected = true, Set.empty[Vertex], Seq.empty[Edge[Vertex]])

  def emptyUndirected[Vertex] = SparseGraph(isDirected = false, Set.empty[Vertex], Seq.empty[Edge[Vertex]])
}
