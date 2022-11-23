%%raw(`import './ASTTree.css';`)

module Tree = {
  type attributeValue

  external intToAttributeValue: int => attributeValue = "%identity"
  external floatToAttributeValue: float => attributeValue = "%identity"
  external boolToAttributeValue: bool => attributeValue = "%identity"
  external stringToAttributeValue: string => attributeValue = "%identity"

  external attributeValueToReactElement: attributeValue => React.element = "%identity"

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

  type onUpdateTarget = {
    node: Js.null<rawNodeDatum>, // supertype of react-d3-tree TreeNodeDatum
    translate: translation, // react-d3-tree Point
    zoom: float,
  }
  type onUpdate = onUpdateTarget => ()

  type dimensions = {height: float, width: float}

  type pathFunctionOption = [ #diagonal | #elbow | #straight | #step ]

  type customNodeElementProps = {
    nodeDatum: rawNodeDatum,
    hierarchyPointNode: {"x": float, "y": float}, // TODO: Get d3 bindings
  }

  type renderCustomNodeElementFn = customNodeElementProps => React.element

  @bs.module("react-d3-tree") @react.component
  external make: (
    ~data: rawNodeDatum,
    ~translate: translation=?,
    ~orientation: orientation=?,
    ~onUpdate: onUpdate=?,
    ~zoom: float=?,
    ~dimensions: dimensions=?,
    ~pathFunc: pathFunctionOption=?, // react-de-tree PathFunctionOption | PathFunction
    ~renderCustomNodeElement: renderCustomNodeElementFn=?
  ) => React.element = "default"
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
    | Some(ty) => others->Js.Dict.set("type", stringToAttributeValue(ty->substitute(constraints)->toFriendlyString))
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
@send external querySelector: (Dom.element, string) => Js.null<Dom.element> = "querySelector"
@bs.get external get_width: Dom.domRect => float = "width"
@bs.get external get_height: Dom.domRect => float = "height"
@send external setAttributeNS: (Dom.element, Js.null<string>, string, string) => () = "setAttributeNS"

module Node = {
  @react.component
  let make = (~nodeDatum: Tree.rawNodeDatum) => {
    open ReactDOM
    let nodeType = nodeDatum.attributes->Js.Dict.get("nodeType")
    let nodeTypeDisplay = switch nodeType {
      | Some(nodeType) => <>{nodeType->Tree.attributeValueToReactElement}<br /></>
      | None => React.null
    }
    let textContainer = React.useRef(Js.Nullable.null)
    let (height, setHeight) = React.useState(_ => 60.0)
    let heightString = height->Belt.Float.toString
    let heightPx = height->Belt.Float.toString ++ "px"
    let translation = `translate(-60, -${(height/.2.0)->Belt.Float.toString})`

    React.useEffect1(() => {
      textContainer.current->Js.Nullable.toOption->Belt.Option.map((dom: Dom.element) => {
        let rect: Dom.domRect = dom->getBoundingClientRect
        let h = rect->get_height
        setHeight(originalHeight => Js.Math.max_float(originalHeight, h))
      })->ignore
      None
    }, [textContainer.current])

    let divStyle = Style.make(~display="flex", ~alignItems="unsafe center", ~justifyContent="unsafe center", ~textAlign="center", ~height=heightPx, ~width="120px", ~overflow="visible", ~overflowWrap="break-word", ())
    <g transform=translation width="120" height=heightString style=Style.make(~overflow="visible", ())>
      <rect width="120" height=heightString fill="#fff" />
      <foreignObject width="120" height=heightString style=Style.make(~overflow="visible", ())>
        <div xmlns="http://www.w3.org/1999/xhtml" style=divStyle>
          <div style=Style.make(~padding="8px", ~width="120px", ()) ref={ReactDOM.Ref.domRef(textContainer)}>
            nodeTypeDisplay
            {nodeDatum.name->React.string}
          </div>
        </div>
      </foreignObject>
    </g>
  }
}

@react.component
let make = (~ast: AST.ast<Type.typeType>, ~constraints: Type.constraints) => {
  open Tree
  let container = React.useRef(Js.Nullable.null)
  let (dimensions, setDimensions) = React.useState(_ => {height: 0.0, width: 0.0})

  React.useEffect1(() => {
    container.current->Js.Nullable.toOption->Belt.Option.map((dom: Dom.element) => {
      let rect: Dom.domRect = dom->getBoundingClientRect
      let treeRect = dom->querySelector("g")->Js.Null.getExn->getBoundingClientRect
      // Center the tree rect.
      setDimensions(_ => {height: rect->get_height, width: rect->get_width})
      let svg = dom->querySelector("svg")->Js.Null.getExn
      svg->setAttributeNS(Js.Null.empty, "viewBox", `${-.(treeRect->get_width/.2.0)->Js.Float.toString} 0 ${treeRect->get_width->Js.Float.toString} ${treeRect->get_height->Js.Float.toString}`)
    })->ignore
    None
  }, [container.current])

  <div id="ast" ref={ReactDOM.Ref.domRef(container)}>
    <Tree data=astToRawNodeDatum(ast, constraints) orientation=#vertical dimensions renderCustomNodeElement=(({nodeDatum}) => <Node nodeDatum />) />
  </div>
}
