%%raw(`import './ASTTree.css';`)

module Tree = {
  open D3.Hierarchy

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

  type treeLinkDatum = {
    source: pointNode<rawNodeDatum>,
    target: pointNode<rawNodeDatum>
  }

  type pathFunction = (treeLinkDatum, orientation) => string
  type pathFunctionOption = [ #diagonal | #elbow | #straight | #step ]

  type pathClassFunction = pathFunction

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
    ~pathClassFunc: pathClassFunction=?,
    ~pathFunc: pathFunctionOption=?, // react-de-tree PathFunctionOption | PathFunction
    ~renderCustomNodeElement: renderCustomNodeElementFn=?
  ) => React.element = "default"
}

let tokenToRawNodeDatum = (token: AST.token): Tree.rawNodeDatum => {
  open Tree
  {attributes: Js.Dict.fromArray([("nodeType", stringToAttributeValue(`${AST.tokenTypeToString(token.type_)} token`)), ("mainValueName", stringToAttributeValue("lexeme"))]), children: [], name: token.lexeme}
}

let makeAttributes = (~ty: option<Type.typeType>=?, ~constraints: Type.constraints=Belt.Map.Int.empty, ~mainValueName: option<string>=?, ~others: Tree.attributeDict=Js.Dict.empty(), nodeType: string): Tree.attributeDict => {
  open Tree
  open Type

  // Copy the dictionary so callers aren't surprised by mutation.
  let others = others->Js.Dict.entries->Js.Dict.fromArray

  others->Js.Dict.set("nodeType", stringToAttributeValue(nodeType))

  switch ty {
    | Some(ty) => others->Js.Dict.set("type", stringToAttributeValue(ty->substitute(constraints)->toFriendlyString))
    | _ => ()
  }

  switch mainValueName {
    | Some(mainValueName) => others->Js.Dict.set("mainValueName", stringToAttributeValue(mainValueName))
    | _ => ()
  }

  others
}

let setInEdgeLabel = (attributes: Tree.attributeDict, label: string) => {
  attributes->Js.Dict.set("inEdgeLabel", label->Tree.stringToAttributeValue)
}

let rec patternToRawNodeDatum = (pat: AST.pattern): Tree.rawNodeDatum => {
  switch pat {
    | PairPat(first, second) => {
      let first = patternToRawNodeDatum(first)
      let second = patternToRawNodeDatum(second)
      {
        attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue("PairPattern"))]),
        children: [first, second],
        name: "(,)"
      }
    }
    | ListPat(patterns) => {
      let patterns = patterns->Js.Array2.map(patternToRawNodeDatum)
      {
        attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue("ListPattern"))]),
        children: patterns,
        name: "[]"
      }
    }
    | Wildcard => {
      attributes: makeAttributes("WildcardPattern"),
      children: [],
      name: "_"
    }
    | ConsPat(head, tail) => {
      let head = patternToRawNodeDatum(head)
      let tail = patternToRawNodeDatum(tail)
      {
        attributes: makeAttributes("ConsPattern"),
        children: [head, tail],
        name: "::"
      }
    }
    | NamePat(name) => {
      {
        attributes: makeAttributes(~mainValueName="name", "NamePattern"),
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
    attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue("Clause"))]),
    children: [pattern, body],
    name: "pattern matching clause"
  }
}
and exprToRawNodeDatum = (e: AST.expr<Type.typeType>, constraints: Type.constraints): Tree.rawNodeDatum => {
  switch e {
    | Match(scrutinee, clauses, ty) => {
      let scrutinee = exprToRawNodeDatum(scrutinee, constraints)
      scrutinee.attributes->setInEdgeLabel("scrutinee")
      let children = clauses->Js.Array2.map(c => clauseToRawNodeDatum(c, constraints))
      children->Js.Array2.unshift(scrutinee)->ignore
      let attributes = makeAttributes(~ty, ~constraints, "Match")
      {attributes, children, name: "match"}
    }
    | Pair(first, second, ty) => {
      let first = exprToRawNodeDatum(first, constraints)
      let second = exprToRawNodeDatum(second, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "Pair"),
        children: [first, second],
        name: "pair"
      }
    }
    | Name(name, ty) => {
      attributes: makeAttributes(~ty, ~constraints, ~mainValueName="name", "Name"),
      children: [],
      name: name.lexeme
    }
    | If(test, ifTrue, ifFalse, ty) => {
      let test = exprToRawNodeDatum(test, constraints)
      test.attributes->setInEdgeLabel("test")
      let ifTrue = exprToRawNodeDatum(ifTrue, constraints)
      ifTrue.attributes->setInEdgeLabel("true branch")
      let ifFalse = exprToRawNodeDatum(ifFalse, constraints)
      ifFalse.attributes->setInEdgeLabel("false branch")
      {
        attributes: makeAttributes(~ty, ~constraints, "If"),
        children: [test, ifTrue, ifFalse],
        name: "if"
      }
    }
    | Rel(left, op, right, ty) => {
      let left = exprToRawNodeDatum(left, constraints)
      let right = exprToRawNodeDatum(right, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, ~mainValueName="operator", "Rel"),
        children: [left, right],
        name: AST.relToFriendlyString(op)
      }
    }
    | Application(fun, arg, ty) => {
      let fun = exprToRawNodeDatum(fun, constraints)
      let arg = exprToRawNodeDatum(arg, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "Application"),
        children: [fun, arg],
        name: "function application"
      }
    }
    | Cons(head, tail, ty) => {
      let head = exprToRawNodeDatum(head, constraints)
      let tail = exprToRawNodeDatum(tail, constraints)
      {
        attributes: makeAttributes(~ty, ~constraints, "Cons"),
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
      let params = params->Js.Array2.map(tokenToRawNodeDatum)
      params->Js.Array2.forEach(param =>
        param.attributes->setInEdgeLabel("param")
      )
      let body = exprToRawNodeDatum(body, constraints)
      body.attributes->setInEdgeLabel("body")
      let children = params
      children->Js.Array2.push(body)->ignore
      let attributes = makeAttributes(~ty, ~constraints, ~mainValueName="name", ~others=Js.Dict.fromArray([("isRec", Tree.boolToAttributeValue(isRec))]), "Let")
      {attributes, children, name: name.lexeme}
    }
  }
}

// Bindings I couldn't find in Webapi
@send external getBBox: Dom.element => Dom.svgRect = "getBBox"

module SVGRect = {
  @get external x: Dom.svgRect => float = "x"
  @get external y: Dom.svgRect => float = "y"
  @get external width: Dom.svgRect => float = "width"
  @get external height: Dom.svgRect => float = "height"
}

module Node = {
  @react.component
  let make = (~nodeDatum: Tree.rawNodeDatum) => {
    open ReactDOM
    open Tree
    open Webapi.Dom
    open Webapi.Dom.Element

    let nodeType = nodeDatum.attributes->Js.Dict.get("nodeType")
    let nodeTypeDisplay = switch nodeType {
      | Some(nodeType) => <>{nodeType->attributeValueToReactElement}<br /></>
      | None => React.null
    }

    let mainValue = switch nodeDatum.attributes->Js.Dict.get("mainValueName") {
      | Some(mainValueName) => <>{mainValueName->attributeValueToReactElement}{" = "->React.string}{nodeDatum.name->Js.Json.serializeExn->React.string}</>
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
        let h = rect->DomRect.height
        setHeight(originalHeight => Js.Math.max_float(originalHeight, h))
      })->ignore
      None
    }, [textContainer.current])

    let divStyle = Style.make(~height=heightPx, ())
    <g transform=translation height=heightString>
      <rect height=heightString />
      <foreignObject height=heightString>
        <div xmlns="http://www.w3.org/1999/xhtml" style=divStyle>
          <div className="text" ref=ReactDOM.Ref.domRef(textContainer)>
            nodeTypeDisplay
            mainValue
          </div>
        </div>
      </foreignObject>
    </g>
  }
}

module LinkCmp = Belt.Id.MakeComparable({
  type t = Tree.treeLinkDatum
  let cmp = (a: t, b: t) => {
    Pervasives.compare(
      (a.source.x, a.source.y, a.target.x, a.target.y),
      (b.source.x, b.source.y, b.target.x, b.target.y)
    )
  }
})

module Label = {
  @react.component
  let make = (~x: float, ~y: float, ~label: Tree.attributeValue) => {
    open Belt.Float

    let labelRef = React.useRef(Js.Nullable.null)
    let (rect, setRect) = React.useState(_ => None)

    React.useEffect1(() => {
      labelRef.current->Js.Nullable.iter((. label) => {
        let rect = label->getBBox
        setRect(_ => Some(rect))
      })
      None
    }, [labelRef.current])

    let background = switch rect {
      | Some(rect) => {
        let backgroundX = rect->SVGRect.x
        let backgroundY = rect->SVGRect.y
        let width = rect->SVGRect.width
        let height = rect->SVGRect.height
        <rect x={backgroundX->toString} y={backgroundY->toString} width={width->toString} height={height->toString} className="label-background" />
      }
      | None => React.null
    }

    <>
      background
      <text x={x->toString} y={y->toString} className="label" ref=ReactDOM.Ref.domRef(labelRef)>{label->Tree.attributeValueToReactElement}</text>
    </>
  }
}

module Labels = {
  @react.component
  let make = (~wrapper: Dom.element, ~links: Belt.Set.Dict.t<Tree.treeLinkDatum, LinkCmp.identity>) => {
    open Belt.Float

    let labels = links->Belt.Set.Dict.toArray->Js.Array2.filter(({target}) => {
      target.data.attributes->Js.Dict.get("inEdgeLabel")->Js.Option.isSome
    })->Js.Array2.map(({source, target}) => {
      let x = (source.x +. target.x)/.2.0
      let y = (source.y +. target.y)/.2.0
      let label = target.data.attributes->Js.Dict.unsafeGet("inEdgeLabel")
      <Label x y label key=`label-${x->toString}-${y->toString}` />
    })

    ReactDOM.createPortal(labels->React.array, wrapper)
  }
}

@react.component
let make = (~ast: AST.ast<Type.typeType>, ~constraints: Type.constraints) => {
  open Tree
  open Webapi.Dom.DomRect
  open Webapi.Dom.Element

  let container = React.useRef(Js.Nullable.null)
  let (dimensions, setDimensions) = React.useState(_ => {height: 0.0, width: 0.0})
  let links = React.useRef(Belt.Set.Dict.empty)

  React.useEffect1(() => {
    container.current->Js.Nullable.toOption->Belt.Option.map((dom: Dom.element) => {
      let rect: Dom.domRect = dom->getBoundingClientRect
      let treeRect = dom->querySelector("g")->Belt.Option.getExn->getBoundingClientRect
      // Center the tree rect.
      setDimensions(_ => {height: rect->height, width: rect->width})
      let svg = dom->querySelector("svg")->Belt.Option.getExn
      svg->setAttributeNS("", "viewBox", `${-.(treeRect->width/.2.0)->Js.Float.toString} 0 ${treeRect->width->Js.Float.toString} ${treeRect->height->Js.Float.toString}`)
    })->ignore
    None
  }, [container.current])

  React.useEffect2(() => {
    Some(() => {
      links.current = Belt.Set.Dict.empty
    })
  }, (ast, constraints))

  <div id="ast" ref={ReactDOM.Ref.domRef(container)}>
    <Tree
      data=astToRawNodeDatum(ast, constraints)
      dimensions
      orientation=#vertical
      pathClassFunc={(link, _) => {
        links.current = links.current->Belt.Set.Dict.add(link, ~cmp=LinkCmp.cmp)
        ".rd3t-link"
      }}
      renderCustomNodeElement=(({nodeDatum}) => <Node nodeDatum />)
    />
    {switch container.current->Js.Nullable.toOption {
      | Some(dom) => {
        let svgWrapperElement = dom->querySelector("g")->Js.Option.getExn
        <Labels wrapper=svgWrapperElement links=links.current />
      }
      | None => React.null
    }}
  </div>
}
