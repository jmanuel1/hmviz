@react.component
let make = (~constraints: Ty.constraints) => {
  <table>
    <thead>
      <tr>
        <th scope="col">{"Variable"->React.string}</th>
        <th scope="col">{"Type"->React.string}</th>
      </tr>
    </thead>
    <tbody>
      {
        constraints->Belt.Map.Int.toArray->Array.map(((var, ty)) => {
          <tr key=Int.toString(var)>
            <td>{Ty.TypeVar(var)->Ty.toFriendlyString->React.string}</td>
            <td>{ty->Ty.toFriendlyString->React.string}</td>
          </tr>
        })->React.array
      }
    </tbody>
  </table>
}
