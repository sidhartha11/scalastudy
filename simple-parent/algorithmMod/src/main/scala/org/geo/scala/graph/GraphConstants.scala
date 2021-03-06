package org.geo.scala.graph

object GraphConstants extends Enumeration {
  val directed = Value("directed graph")
  val undirected = Value("undirected graph")
  val adj_linked = Value("Using Linked Allocation")
  val adj_map = Value("Using Mapped Allocation")
  val adj_unim = Value("Feature unimplemented")
  val recursive = Value("recursive")
  val non_recursive = Value("non_recursive")
  val synthetic = Value("synthetic")
  val nonsynthetic = Value("nonsynthetic")
}