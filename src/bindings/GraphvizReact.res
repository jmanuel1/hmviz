type graphvizOptions = {
  width: Js.nullable<int>,
  height: Js.nullable<int>
}

@bs.module("graphviz-react") @react.component
external make: (
  ~dot: string,
  ~options: graphvizOptions=?,
  ~className: string=?
) => React.element = "Graphviz"
