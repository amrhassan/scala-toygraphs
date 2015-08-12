package info.amrhassan.graphs

import org.scalatest.FunSuite

class PathTest extends FunSuite {

  test("checks valid for paths") {

    intercept[IllegalArgumentException] {
      Path(Seq(
        Edge("a", "b"), Edge("b", "c"), Edge("b", "d")
      ))
    }

    Path(Seq(
      Edge("a", "b"), Edge("b", "c"), Edge("c", "d")
    ))

  }
}
