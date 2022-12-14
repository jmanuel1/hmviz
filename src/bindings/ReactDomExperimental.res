type root

@bs.val @bs.return(nullable)
external _getElementById: string => option<Dom.element> =
  "document.getElementById"

@bs.module("react-dom")
external createRoot: Dom.element => root = "createRoot"

let createRootWithId = id =>
  switch _getElementById(id) {
  | None => None
  | Some(element) => Some(createRoot(element))
  }

@bs.module("react-dom")
external createBlockingRoot: Dom.element => root = "createBlockingRoot"

let createBlockingRootWithId = id =>
  switch _getElementById(id) {
  | None => None
  | Some(element) => Some(createBlockingRoot(element))
  }

@bs.send external render: (root, React.element) => unit = "render"
