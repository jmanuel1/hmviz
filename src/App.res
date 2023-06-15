%%raw(`import './App.css';`)

@react.component
let make = () => {
  open AST
  open Type
  open ArrayUtil

  let (codeInput, setCodeInput) = React.useState(_ => "")
  let (inferenceSteps, setInferenceSteps) = React.useState(_ => [])
  let (inferenceStepIndex, setInferenceStepIndex) = React.useState(_ => 0)
  let typedAST = inferenceSteps->Belt.Array.get(inferenceStepIndex)->Belt.Option.map(s => s.typedAST)
  let constraints = inferenceSteps->Belt.Array.get(inferenceStepIndex)->Belt.Option.map(s => s.constraints)->Belt.Option.getWithDefault(Belt.Map.Int.empty)

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
          setInferenceSteps(_ => steps)
          setInferenceStepIndex(_ => 0)
        }
        | Error(msg, state) => {
          Js.log(`expected ${msg}, saw following token at token index ${state.index->Belt.Int.toString}:`)
          Js.log(tokens->Js.Array2.slice(~start=state.index, ~end_=state.index + 5))
        }
      }
    })->ignore
  }

  let goToPreviousStep = event => {
    setInferenceStepIndex(i => i - 1)
  }

  let goToNextStep = event => {
    setInferenceStepIndex(i => i + 1)
  }

  <div className="App">
    <form onSubmit=onSubmitCodeInput>
      <label>
        {React.string("Enter an OCaml expression or declaration")}
        <textarea value=codeInput onChange=onCodeInputChange />
      </label>
      <div>
        <button type_="submit">{React.string("Infer types")}</button>
      </div>
    </form>
    <div>
      {
        if inferenceSteps->Js.Array2.length > 0 {
          React.string(`Step ${(inferenceStepIndex + 1)->Js.String2.make}`)
        } else {
          React.string("")
        }
      }
      <button onClick=goToPreviousStep disabled={inferenceStepIndex <= 0}>{React.string("Previous step")}</button>
      <button onClick=goToNextStep disabled={inferenceStepIndex >= inferenceSteps->Js.Array2.length - 1}>{React.string("Next step")}</button>
    </div>
    {
      switch typedAST {
        | Some(typedAST) => <ASTTree ast=typedAST constraints />
        | None => React.null
      }
    }
  </div>
}
