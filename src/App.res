%%raw(`import './App.css';`)

@react.component
let make = () => {
  open AST
  open Ty

  let (codeInput, setCodeInput) = React.useState(_ => "")
  let (inferenceSteps, setInferenceSteps) = React.useState(_ => [])
  let (inferenceStepIndex, setInferenceStepIndex) = React.useState(_ => 0)
  let typedAST = inferenceSteps->Array.get(inferenceStepIndex)->Option.map(s => s.typedAST)
  let constraints = inferenceSteps->Array.get(inferenceStepIndex)->Option.map(s => s.constraints)->Option.getOr(Belt.Map.Int.empty)

  let onCodeInputChange = event => {
    ReactEvent.Form.preventDefault(event)
    let value = ReactEvent.Form.target(event)["value"]
    setCodeInput(_ => value)
  }

  let onSubmitCodeInput = event => {
    ReactEvent.Form.preventDefault(event)
    let tokens = tokenize(codeInput)

    tokens->Option.map(tokens => {
      let ast = parse(tokens)
      switch ast {
        | Ok(ast) => {
          let steps = generateInferenceSteps(ast)->TracedState.toStateArray
          steps->Array.forEach(step => {
            Console.log("step:")
            Console.log(step)
            Console.log("constraints:")
            step.constraints->Belt.Map.Int.forEach((v, t) => {
              Console.log(`var: ${v->Int.toString}, type: ${typeToString(t)}`)
            })
          })
          setInferenceSteps(_ => steps)
          setInferenceStepIndex(_ => 0)
        }
        | Error(msg, state) => {
          Console.log(`expected ${msg}, saw following token at token index ${state.index->Int.toString}:`)
          Console.log(tokens->Array.slice(~start=state.index, ~end=state.index + 5))
        }
      }
    })->ignore
  }

  let goToFirstStep = event => {
    setInferenceStepIndex(i => 0)
  }

  let goToPreviousStep = event => {
    setInferenceStepIndex(i => i - 1)
  }

  let goToNextStep = event => {
    setInferenceStepIndex(i => i + 1)
  }

  let goToLastStep = event => {
    setInferenceStepIndex(i => inferenceSteps->Array.length - 1)
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
        if inferenceSteps->Array.length > 0 {
          React.string(`Step ${(inferenceStepIndex + 1)->String.make}`)
        } else {
          React.string("")
        }
      }
      // TODO: Replace with a good pagination component?
      <button onClick=goToFirstStep disabled={inferenceStepIndex <= 0}>{React.string("First step")}</button>
      <button onClick=goToPreviousStep disabled={inferenceStepIndex <= 0}>{React.string("Previous step")}</button>
      <button onClick=goToNextStep disabled={inferenceStepIndex >= inferenceSteps->Array.length - 1}>{React.string("Next step")}</button>
      <button onClick=goToLastStep disabled={inferenceStepIndex >= inferenceSteps->Array.length - 1}>{React.string("Last step")}</button>
    </div>
    <ConstraintTable constraints />
    {
      switch typedAST {
        | Some(typedAST) => <ASTTree ast=typedAST constraints />
        | None => React.null
      }
    }
  </div>
}
