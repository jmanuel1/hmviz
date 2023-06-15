%%raw(`import './ASTTree.css';`)

open GraphvizBuilder
open GraphvizRecordLabel

let nextID : ref<int> = ref(0)

let freshID = () : string => {
  let id = nextID.contents
  nextID := nextID.contents + 1
  id->Js.String2.make
}

let addUniqueNode = (graph: graph, label: string): Node.t => {
  let attrs = Js.Dict.fromArray([("label", label), ("shape", "record"), ("fontname", "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen',
    'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
    sans-serif")])
  graph->addNodeWithAttributes(freshID(), attrs)
}

let addTokenNode = (graph: graph, token: AST.token): Node.t => {
  let fields = [
    flipLayout(makeRecordLabel([
      boxLabelToField(makeBoxLabel(~text=`${AST.tokenTypeToString(token.type_)} token`, ())),
      boxLabelToField(makeBoxLabel(~text=token.lexeme, ()))
    ]))
  ]
  let node = graph->addUniqueNode(makeRecordLabel(fields))
  node
}

let addLabeledEdge = (graph: graph, source: Node.t, target: Node.t, label: string) => {
  let edge = graph->addEdge(source, target)
  edge->Edge.set("label", label)
  edge
}

let rec addPatternNode = (graph: graph, pat: AST.pattern): Node.t => {
  let makeNode = (fields: array<string>): Node.t => {
    graph->addUniqueNode(makeRecordLabel([flipLayout(makeRecordLabel(fields))]))
  }
  switch pat {
    | PairPat(first, second) => {
      let first = addPatternNode(graph, first)
      let second = addPatternNode(graph, second)
      let fields = [
        "Pair Pattern",
        "(,)"
      ]
      let node = makeNode(fields)
      graph->addEdge(node, first)->ignore
      graph->addEdge(node, second)->ignore
      node
    }
    | ListPat(patterns) => {
      let patterns = patterns->Js.Array2.map(addPatternNode(graph))
      let fields = [
        "List Pattern",
        "[]"
      ]
      let node = makeNode(fields)
      patterns->Js.Array2.forEach(pat => graph->addEdge(node, pat)->ignore)
      node
    }
    | Wildcard => {
      let fields = [
        "Wildcard Pattern",
        "_"
      ]
      makeNode(fields)
    }
    | ConsPat(head, tail) => {
      let head = graph->addPatternNode(head)
      let tail = graph->addPatternNode(tail)
      let fields = [
        "Cons Pattern",
        "(::)"
      ]
      let node = makeNode(fields)
      graph->addEdge(node, head)->ignore
      graph->addEdge(node, tail)->ignore
      node
    }
    | NamePat(name) => {
      let fields = [
        "Name Pattern",
        boxLabelToField(makeBoxLabel(~text=`name = ${name.lexeme}`, ()))
      ]
      makeNode(fields)
    }
  }
}

let addTypedNode = (graph: graph, id: string, ty: Type.typeType, constraints: Type.constraints): Node.t => {
  open Type

  graph->addUniqueNode(makeRecordLabel([flipLayout(makeRecordLabel([id, boxLabelToField(makeBoxLabel(~text=`type = ${ty->substitute(constraints)->toFriendlyString}`, ()))]))]))
}

let rec addClauseNode = (graph: graph, clause: AST.clause<Type.typeType>, constraints: Type.constraints): Node.t => {
  let (pattern, body) = clause
  let pattern = graph->addPatternNode(pattern)
  let body = graph->addExprNode(body, constraints)
  let node = graph->addUniqueNode("Pattern Match Clause")
  graph->addEdge(node, pattern)->ignore
  graph->addEdge(node, body)->ignore
  node
}
and addExprNode = (graph: graph, e: AST.expr<Type.typeType>, constraints: Type.constraints): Node.t => {
  open AST

  switch e {
    | Match(scrutinee, clauses, ty) => {
      let node = graph->addTypedNode("Pattern Match", ty, constraints)
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
      graph->addTypedNode(`Name: ${name.lexeme}`, ty, constraints)
    }
    | If(test, ifTrue, ifFalse, ty) => {
      let node = graph->addTypedNode("If", ty, constraints)
      let test = graph->addExprNode(test, constraints)
      graph->addLabeledEdge(node, test, "test")->ignore
      let ifTrue = graph->addExprNode(ifTrue, constraints)
      graph->addLabeledEdge(node, ifTrue, "true branch")->ignore
      let ifFalse = graph->addExprNode(ifFalse, constraints)
      graph->addLabeledEdge(node, ifFalse, "false branch")->ignore
      node
    }
    | Rel(left, op, right, ty) => {
      let left = graph->addExprNode(left, constraints)
      let right = graph->addExprNode(right, constraints)
      let fields = [
        "Rel",
        boxLabelToField(makeBoxLabel(~text=`operator = ${op->relToFriendlyString}`, ()))
      ]
      let node = graph->addTypedNode(makeRecordLabel(fields), ty, constraints)
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
      let fields = [
        "Cons",
        "(::)"
      ]
      let node = graph->addTypedNode(makeRecordLabel(fields), ty, constraints)
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
      let fields = [
        "Let",
        boxLabelToField(makeBoxLabel(~text=`name = ${name.lexeme}`, ())),
        boxLabelToField(makeBoxLabel(~text=`recursive? = ${isRec->Js.String2.make}`, ()))
      ]
      let node = graph->addTypedNode(makeRecordLabel(fields), ty, constraints)
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
