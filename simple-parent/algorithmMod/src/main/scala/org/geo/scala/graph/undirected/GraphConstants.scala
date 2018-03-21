package org.geo.scala.graph.undirected

object GraphConstants extends Enumeration {
  val directed = Value("directed graph")
  val undirected = Value("undirected graph")
  val adj_linked = Value("Using Linked Allocation")
  val adj_map = Value("Using Mapped Allocation")
  val adj_unim = Value("Feature unimplemented")
}