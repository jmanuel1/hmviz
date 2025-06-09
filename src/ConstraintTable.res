@react.component
let make = (~constraints: Type.constraints) => {
  <table>
    <thead>
      <tr>
        <th scope="col">{"Variable"->React.string}</th>
        <th scope="col">{"Type"->React.string}</th>
      </tr>
    </thead>
    <tbody>
      {
        constraints->Belt.Map.Int.toArray->Js.Array2.map(((var, ty)) => {
          <tr key=Belt.Int.toString(var)>
            <td>{Type.TypeVar(var)->Type.toFriendlyString->React.string}</td>
            <td>{ty->Type.toFriendlyString->React.string}</td>
          </tr>
        })->React.array
      }
    </tbody>
  </table>
}
