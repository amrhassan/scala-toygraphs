package info.amrhassan.graphs

case class Edge[Vertex](v1: Vertex, v2: Vertex) {

  val from = v1

  val to = v2

  lazy val vertexSet: Set[Vertex] = Set(v1, v2)

  lazy val vertices: Traversable[Vertex] = Seq(v1, v2)


  /**
   * Returns the other vertex that's not the given one.
   */
  def otherVertex(v: Vertex): Vertex =
    if (v == v1) v2 else v1
}
