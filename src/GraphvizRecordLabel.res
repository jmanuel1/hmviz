let makeRecordLabel = (fields: array<string>): string => {
  fields->Array.joinWith(" | ")
}

let boxLabelToField = (boxLabel: string): string => boxLabel
let flipLayout = (recordLabel: string): string => `{ ${recordLabel} }`

let makeBoxLabel = (~port: option<string>=?, ~text: option<string>=?, ()): string => {
  let port = switch port {
    | Some(port) => `<${port}>`
    | None => ""
  }
  let text = switch text {
    | Some(text) => text->String.replaceRegExp(%re("/([{}|<> ])/g"), "\\$1")
    | None => ""
  }
  `${port} ${text}`
}
