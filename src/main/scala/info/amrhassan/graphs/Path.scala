package info.amrhassan.graphs

case class Path[Vertex](edges: Seq[Edge[Vertex]]) {
  require {
    val dests = (edges map(_.to)) dropRight 1
    val sources = (edges drop 1) map(_.from)
    dests == sources
  }

  lazy val vertices: Seq[Vertex] = edges map (_.from)

  lazy val length: Double = (edges map (_.length)).sum
}
