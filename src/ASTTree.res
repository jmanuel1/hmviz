%%raw(`import './ASTTree.css';`)

module Tree = {
  type attributeValue

  external intToAttributeValue: int => attributeValue = "%identity"
  external floatToAttributeValue: float => attributeValue = "%identity"
  external boolToAttributeValue: bool => attributeValue = "%identity"
  external stringToAttributeValue: string => attributeValue = "%identity"

  type attributeDict = Js.Dict.t<attributeValue>

  type rec rawNodeDatum = {
    attributes: attributeDict,
    children: array<rawNodeDatum>,
    name: string
  }

  type translation = {
    x: float,
    y: float
  }

  type orientation = [ #horizontal | #vertical ]

  @bs.module("react-d3-tree") @react.component
  external make: (~data: rawNodeDatum, ~translate: translation=?, ~orientation: orientation=?) => React.element = "default"
}

let tokenToRawNodeDatum = (token: AST.token): Tree.rawNodeDatum => {
  {attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue(`${AST.tokenTypeToString(token.type_)} token`))]), children: [], name: token.lexeme}
}

let makeAttributes = (~ty: option<Type.typeType>=?, ~constraints: Type.constraints=Belt.Map.Int.empty, ~others: Tree.attributeDict=Js.Dict.empty(), nodeType: string): Tree.attributeDict => {
  open Tree
  open Type

  // Copy the dictionary so callers aren't surprised by mutation.
  let others = others->Js.Dict.entries->Js.Dict.fromArray

  others->Js.Dict.set("nodeType", stringToAttributeValue(nodeType))
  switch ty {
    | Some(ty) => others->Js.Dict.set("type", stringToAttributeValue(typeToString(ty->substitute(constraints))))
    | _ => ()
  }

  others
}

let rec patternToRawNodeDatum = (pat: AST.pattern): Tree.rawNodeDatum => {
  switch pat {
    | PairPat(first, second) => {
      let first = patternToRawNodeDatum(first)
      let second = patternToRawNodeDatum(second)
      {
        attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue("pairPattern"))]),
        children: [first, second],
        name: "(,)"
      }
    }
    | ListPat(patterns) => {
      let patterns = patterns->Js.Array2.map(patternToRawNodeDatum)
      {
        attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue("listPattern"))]),
        children: patterns,
        name: "[]"
      }
    }
    | Wildcard => {
      attributes: makeAttributes("wildcardPattern"),
      children: [],
      name: "_"
    }
    | ConsPat(head, tail) => {
      let head = patternToRawNodeDatum(head)
      let tail = patternToRawNodeDatum(tail)
      {
        attributes: makeAttributes("consPattern"),
        children: [head, tail],
        name: "::"
      }
    }
    | NamePat(name) => {
      {
        attributes: makeAttributes("namePattern"),
        children: [],
        name: name.lexeme
      }
    }
  }
}

let rec clauseToRawNodeDatum = (clause: AST.clause<Type.typeType>, constraints: Type.constraints): Tree.rawNodeDatum => {
  let (pattern, body) = clause
  let pattern = patternToRawNodeDatum(pattern)
  let body = exprToRawNodeDatum(body, constraints)
  {
    attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue("clause"))]),
    children: [pattern, body],
    name: "pattern matching clause"
  }
}
and exprToRawNodeDatum = (e: AST.expr<Type.typeType>, constraints: Type.constraints): Tree.rawNodeDatum => {
  switch e {
    | Match(scrutinee, clauses, ty) => {
      let scrutinee = exprToRawNodeDatum(scrutinee, constraints)
      let children = clauses->Js.Array2.map(c => clauseToRawNodeDatum(c, constraints))
      children->Js.Array2.unshift(scrutinee)->ignore
      let attributes = makeAttributes(~ty, ~constraints, "match")
      {attributes, children, name: "match"}
    }
    | Pair(first, second, ty) => {
      let first = exprToRawNodeDatum(first, constraints)
      let second = exprToRawNodeDatum(second, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "pair"),
        children: [first, second],
        name: "pair"
      }
    }
    | Name(name, ty) => {
      attributes: makeAttributes(~ty, ~constraints, "name"),
      children: [],
      name: name.lexeme
    }
    | If(test, ifTrue, ifFalse, ty) => {
      let test = exprToRawNodeDatum(test, constraints)
      let ifTrue = exprToRawNodeDatum(ifTrue, constraints)
      let ifFalse = exprToRawNodeDatum(ifFalse, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "if"),
        children: [test, ifTrue, ifFalse],
        name: "if"
      }
    }
    | Rel(left, op, right, ty) => {
      let left = exprToRawNodeDatum(left, constraints)
      let right = exprToRawNodeDatum(right, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "rel"),
        children: [left, right],
        name: AST.relToFriendlyString(op)
      }
    }
    | Application(fun, arg, ty) => {
      let fun = exprToRawNodeDatum(fun, constraints)
      let arg = exprToRawNodeDatum(arg, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "app"),
        children: [fun, arg],
        name: "function application"
      }
    }
    | Cons(head, tail, ty) => {
      let head = exprToRawNodeDatum(head, constraints)
      let tail = exprToRawNodeDatum(tail, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "cons"),
        children: [head, tail],
        name: "::"
      }
    }
  }
}

let astToRawNodeDatum = (ast: AST.ast<Type.typeType>, constraints: Type.constraints): Tree.rawNodeDatum => {
  open AST

  switch ast {
    | Let(name, isRec, params, body, ty) => {
      let children = params->Js.Array2.map(tokenToRawNodeDatum)
      children->Js.Array2.push(exprToRawNodeDatum(body, constraints))->ignore
      let attributes = makeAttributes(~ty, ~constraints, ~others=Js.Dict.fromArray([("rec", Tree.boolToAttributeValue(isRec))]), "let")
      {attributes, children, name: name.lexeme}
    }
  }
}

@send external getBoundingClientRect: Dom.element => Dom.domRect = "getBoundingClientRect"
@bs.get external get_width: Dom.domRect => int = "width"
@bs.get external get_height: Dom.domRect => int = "height"

@react.component
let make = (~ast: AST.ast<Type.typeType>, ~constraints: Type.constraints) => {
  open Tree
  let (translation, setTranslation) = React.useState(_ => {x: 0, y: 0})
  let container = React.useRef(Js.Nullable.null)

  React.useEffect1(() => {
    container.current->Js.Nullable.toOption->Belt.Option.map((dom: Dom.element) => {
      let rect: Dom.domRect = dom->getBoundingClientRect
      setTranslation(_ => {x: rect->get_width / 2, y: rect->get_height / 2})
    })->ignore
    None
  }, [container.current])

  <div id="ast" ref={ReactDOM.Ref.domRef(container)}>
    <Tree data=astToRawNodeDatum(ast, constraints) orientation=#vertical dimensions />
  </div>
}
