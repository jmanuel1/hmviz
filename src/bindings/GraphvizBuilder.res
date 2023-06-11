type graph

module Node = {
  type t

  @send
  external set: (t, string, string) => () = "set"
}

@bs.module("graphviz-builder")
external digraph: string => graph = "digraph"

@send
external addNodeWithAttributes: (graph, string, Js.Dict.t<string>) => Node.t = "addNode"

@send
external addNode: (graph, string) => Node.t = "addNode"

module Edge = {
  type t

  @send
  external set: (t, string, string) => () = "set"
}

@send
external addEdge: (graph, Node.t, Node.t) => Edge.t = "addEdge"

@send
external toDot: graph => string = "to_dot"
