type root

@bs.module("react-dom")
external createRoot: Dom.element => root = "createRoot"

let createRootWithId: string => option<root>

@bs.module("react-dom")
external createBlockingRoot: Dom.element => root = "createBlockingRoot"

let createBlockingRootWithId: string => option<root>

@bs.send external render: (root, React.element) => unit = "render"
