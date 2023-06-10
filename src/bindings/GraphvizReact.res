type graphvizOptions

@bs.module("graphviz-react") @react.component
external make: (
  ~dot: string,
  ~options: graphvizOptions=?,
  ~className: string=?
) => React.element = "Graphviz"
