%%raw(`import './ASTTree.css';`)

open GraphvizBuilder

let nextID : ref<int> = ref(0)

let freshID = () : string => {
  let id = nextID.contents
  nextID := nextID.contents + 1
  id->Js.String2.make
}

let addUniqueNode = (graph: graph, label: string): Node.t => {
  graph->addNodeWithAttributes(freshID(), Js.Dict.fromArray([("label", label)]))
}

let addTokenNode = (graph: graph, token: AST.token): Node.t => {
  let node = graph->addUniqueNode(`${AST.tokenTypeToString(token.type_)} token\n${token.lexeme}`)
  node
}

let addLabeledEdge = (graph: graph, source: Node.t, target: Node.t, label: string) => {
  let edge = graph->addEdge(source, target)
  edge->Edge.set("label", label)
  edge
}

let rec addPatternNode = (graph: graph, pat: AST.pattern): Node.t => {
  switch pat {
    | PairPat(first, second) => {
      let first = addPatternNode(graph, first)
      let second = addPatternNode(graph, second)
      let node = graph->addUniqueNode("PairPattern\n(,)")
      graph->addEdge(node, first)->ignore
      graph->addEdge(node, second)->ignore
      node
    }
    | ListPat(patterns) => {
      let patterns = patterns->Js.Array2.map(addPatternNode(graph))
      let node = graph->addUniqueNode("ListPattern\n[]")
      patterns->Js.Array2.forEach(pat => graph->addEdge(node, pat)->ignore)
      node
    }
    | Wildcard => {
      graph->addUniqueNode("WildcardPattern\n_")
    }
    | ConsPat(head, tail) => {
      let head = graph->addPatternNode(head)
      let tail = graph->addPatternNode(tail)
      let node = graph->addUniqueNode("ConsPattern\n::")
      graph->addEdge(node, head)->ignore
      graph->addEdge(node, tail)->ignore
      node
    }
    | NamePat(name) => {
      graph->addUniqueNode(`NamePattern\nname = ${name.lexeme}`)
    }
  }
}

let addTypedNode = (graph: graph, id: string, ty: Type.typeType, constraints: Type.constraints): Node.t => {
  open Type

  graph->addUniqueNode(`${id}\ntype = ${ty->substitute(constraints)->toFriendlyString}`)
}

let rec addClauseNode = (graph: graph, clause: AST.clause<Type.typeType>, constraints: Type.constraints): Node.t => {
  let (pattern, body) = clause
  let pattern = graph->addPatternNode(pattern)
  let body = graph->addExprNode(body, constraints)
  let node = graph->addUniqueNode("Clause\npattern matching clause")
  graph->addEdge(node, pattern)->ignore
  graph->addEdge(node, body)->ignore
  node
}
and addExprNode = (graph: graph, e: AST.expr<Type.typeType>, constraints: Type.constraints): Node.t => {
  open AST

  switch e {
    | Match(scrutinee, clauses, ty) => {
      let node = graph->addTypedNode("Match", ty, constraints)
      let scrutinee = graph->addExprNode(scrutinee, constraints)
      graph->addLabeledEdge(node, scrutinee, "scrutinee")->ignore
      let children = clauses->Js.Array2.map(c => graph->addClauseNode(c, constraints))
      children->Js.Array2.forEach(clause => graph->addEdge(node, clause)->ignore)
      node
    }
    | Pair(first, second, ty) => {
      let first = graph->addExprNode(first, constraints)
      let second = graph->addExprNode(second, constraints)
      let node = graph->addTypedNode("Pair", ty, constraints)
      graph->addEdge(node, first)->ignore
      graph->addEdge(node, second)->ignore
      node
    }
    | Name(name, ty) => {
      graph->addTypedNode(`Name\n${name.lexeme}`, ty, constraints)
    }
    | If(test, ifTrue, ifFalse, ty) => {
      let node = graph->addTypedNode("If", ty, constraints)
      let test = graph->addExprNode(test, constraints)
      graph->addLabeledEdge(node, test, "test")->ignore
      let ifTrue = graph->addExprNode(ifTrue, constraints)
      graph->addLabeledEdge(node, ifTrue, "true branch")->ignore
      let ifFalse = graph->addExprNode(ifFalse, constraints)
      graph->addLabeledEdge(node, ifFalse,"false branch")->ignore
      node
    }
    | Rel(left, op, right, ty) => {
      let left = graph->addExprNode(left, constraints)
      let right = graph->addExprNode(right, constraints)
      let node = graph->addTypedNode(`Rel\noperator = ${op->relToFriendlyString}`, ty, constraints)
      graph->addEdge(node, left)->ignore
      graph->addEdge(node, right)->ignore
      node
    }
    | Application(fun, arg, ty) => {
      let fun = graph->addExprNode(fun, constraints)
      let arg = graph->addExprNode(arg, constraints)
      let node = graph->addTypedNode("Application", ty, constraints)
      graph->addEdge(node, fun)->ignore
      graph->addEdge(node, arg)->ignore
      node
    }
    | Cons(head, tail, ty) => {
      let head = graph->addExprNode(head, constraints)
      let tail = graph->addExprNode(tail, constraints)
      let node = graph->addTypedNode("Cons\n::", ty, constraints)
      graph->addEdge(node, head)->ignore
      graph->addEdge(node, tail)->ignore
      node
    }
  }
}

let addASTNode = (graph: graph, ast: AST.ast<Type.typeType>, constraints: Type.constraints): Node.t => {
  open AST

  switch ast {
    | Let(name, isRec, params, body, ty) => {
      let node = graph->addTypedNode(`Let\nname = ${name.lexeme}\nrecursive? = ${isRec->Js.String2.make}`, ty, constraints)
      let params = params->Js.Array2.map(graph->addTokenNode)
      params->Js.Array2.forEach(param =>
        graph->addLabeledEdge(node, param, "parameter")->ignore
      )
      let body = graph->addExprNode(body, constraints)
      graph->addLabeledEdge(node, body, "body")->ignore
      node
    }
  }
}

@react.component
let make = (~ast: AST.ast<Type.typeType>, ~constraints: Type.constraints) => {
  open GraphvizReact

  let graph = digraph("AST")
  graph->addASTNode(ast, constraints)->ignore

  let dot = graph->toDot

  <div id="ast">
    <GraphvizReact dot />
  </div>
}
