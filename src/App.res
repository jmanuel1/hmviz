%%raw(`import './App.css';`)

@react.component
let make = () => {
  open AST
  open Type
  open ArrayUtil

  let (codeInput, setCodeInput) = React.useState(_ => "")
  let (typedAST, setTypedAST) = React.useState(_ => None)
  let (constraints, setConstraints) = React.useState(_ => Belt.Map.Int.empty)

  let onCodeInputChange = event => {
    ReactEvent.Form.preventDefault(event)
    let value = ReactEvent.Form.target(event)["value"]
    setCodeInput(_ => value)
  }

  let onSubmitCodeInput = event => {
    ReactEvent.Form.preventDefault(event)
    let tokens = tokenize(codeInput)

    tokens->Belt.Option.map(tokens => {
      let ast = parse(tokens)
      switch ast {
        | Ok(ast) => {
          let steps = generateInferenceSteps(ast)->TracedState.toStateArray
          steps->Js.Array2.forEach(step => {
            Js.log("step:")
            Js.log(step)
            Js.log("constraints:")
            step.constraints->Belt.Map.Int.forEach((v, t) => {
              Js.log(`var: ${v->Belt.Int.toString}, type: ${typeToString(t)}`)
            })
          })
          setTypedAST(_ => Some((steps->last).typedAST))
          setConstraints(_ => (steps->last).constraints)
        }
        | Error(msg, state) => {
          Js.log(`expected ${msg}, saw following token at token index ${state.index->Belt.Int.toString}:`)
          Js.log(tokens->Js.Array2.slice(~start=state.index, ~end_=state.index + 5))
        }
      }
    })->ignore
  }

  <div className="App">
    <form onSubmit=onSubmitCodeInput>
      <label>
        {React.string("Enter an OCaml expression or declaration")}
        <textarea value=codeInput onChange=onCodeInputChange />
      </label>
      <button type_="submit">{React.string("Infer types")}</button>
    </form>
    {
      switch typedAST {
        | Some(typedAST) => <ASTTree ast=typedAST constraints />
        | None => React.null
      }
    }
  </div>
}
