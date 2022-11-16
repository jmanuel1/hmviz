%%raw(`import './ASTTree.css';`)

module Tree = {
  type attributeValue

  external intToAttributeValue: int => attributeValue = "%identity"
  external floatToAttributeValue: float => attributeValue = "%identity"
  external boolToAttributeValue: bool => attributeValue = "%identity"
  external stringToAttributeValue: string => attributeValue = "%identity"

  type rec rawNodeDatum = {
    attributes: Js.Dict.t<attributeValue>,
    children: array<rawNodeDatum>,
    name: string
  }

  type translation = {
    x: int,
    y: int
  }

  type orientation = [ #horizontal | #vertical ]

  @bs.module("react-d3-tree") @react.component
  external make: (~data: rawNodeDatum, ~translate: translation=?, ~orientation: orientation=?) => React.element = "default"
}

let tokenToRawNodeDatum = (token: AST.token): Tree.rawNodeDatum => {
  {attributes: Js.Dict.fromArray([("nodeType", Tree.stringToAttributeValue(`${AST.tokenTypeToString(token.type_)} token`))]), children: [], name: token.lexeme}
}

let exprToRawNodeDatum = (e: AST.expr<Type.typeType>): Tree.rawNodeDatum => {
  {attributes: Js.Dict.fromArray([]), children: [], name: "expr"}
}

let astToRawNodeDatum: AST.ast<Type.typeType> => Tree.rawNodeDatum = ast => {
  open AST

  switch ast {
    | Let(name, isRec, params, body, ty) => {
      let children = params->Js.Array2.map(tokenToRawNodeDatum)
      children->Js.Array2.push(exprToRawNodeDatum(body))->ignore
      {attributes: Js.Dict.fromArray([("rec", Tree.boolToAttributeValue(isRec)), ("type", Tree.stringToAttributeValue(Type.typeToString(ty))), ("nodeType", Tree.stringToAttributeValue("let"))]), children, name: name.lexeme}
    }
  }
}

@send external getBoundingClientRect: Dom.element => Dom.domRect = "getBoundingClientRect"
@bs.get external get_width: Dom.domRect => int = "width"
@bs.get external get_height: Dom.domRect => int = "height"

@react.component
let make = (~ast: AST.ast<Type.typeType>) => {
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
    <Tree data=astToRawNodeDatum(ast) translate=translation orientation=#vertical />
  </div>
}
