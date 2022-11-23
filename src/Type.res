type typeVar = int

type rec typeType = TypeVar(typeVar) | TypeInt | TypeFloat | TypeString | TypeFun(typeType, typeType) | TypePair(typeType, typeType) | TypeList(typeType) | TypeBool

let rec typeToString : typeType => string = ty => {
  switch ty {
    | TypeVar(v) => `TypeVar(${v->Belt.Int.toString})`
    | TypeInt => "TypeInt"
    | TypeFloat => "TypeFloat"
    | TypeString => "TypeString"
    | TypeFun(arg, ret) => `TypeFun(${typeToString(arg)}, ${typeToString(ret)})`
    | TypePair(first, second) => `TypePair(${typeToString(first)}, ${typeToString(second)})`
    | TypeList(el) => `TypeList(${typeToString(el)})`
    | TypeBool => "TypeBool"
  }
}

let rec toFriendlyString = (ty: typeType): string => {
  switch ty {
    | TypeVar(v) => `'t${v->Belt.Int.toString}`
    | TypeInt => "int"
    | TypeFloat => "float"
    | TypeString => "string"
    | TypeFun(arg, ret) => {
      let argString = switch arg {
        | TypeFun(_, _) => `(${toFriendlyString(arg)})`
        | _ => toFriendlyString(arg)
      }
      `${argString} -> ${toFriendlyString(ret)}`
    }
    | TypePair(first, second) => `(${toFriendlyString(first)}, ${toFriendlyString(second)})`
    | TypeList(el) => `${toFriendlyString(el)} list`
    | TypeBool => "bool"
  }
}

type context = Belt.Map.String.t<typeType>

type constraints = Belt.Map.Int.t<typeType>

type inferenceState = {
  constraints : constraints,
  nextTypeVar : typeVar,
  typedAST : AST.ast<typeType>,
  context: context
}

let annotateWithTypeVars : (AST.ast<'ann>, typeVar) => (AST.ast<typeType>, typeVar) = (ast, nextTypeVar) => {
  open AST

  let rec annotateExpr : (expr<'ann>, typeVar) => (expr<typeType>, typeVar) = (expr, nextTypeVar) => {
    switch expr {
      | Match(scrutinee, clauses, _) => {
        let (typedScrutinee, nextTypeVar) = annotateExpr(scrutinee, nextTypeVar)
        let (typedClauses, nextTypeVar) = clauses->Js.Array2.reduce(((typedClauses, nextTypeVar), (pattern, body)) => {
          let (typedClauseBody, nextTypeVar) = annotateExpr(body, nextTypeVar)
          typedClauses->Js.Array2.push((pattern, typedClauseBody))->ignore
          (typedClauses, nextTypeVar)
        }, ([], nextTypeVar))
        (Match(typedScrutinee, typedClauses, TypeVar(nextTypeVar)), nextTypeVar + 1)
      }
      | Pair(first, second, _) => {
        let (typedFirst, nextTypeVar) = annotateExpr(first, nextTypeVar)
        let (typedSecond, nextTypeVar) = annotateExpr(second, nextTypeVar)
        (Pair(typedFirst, typedSecond, TypeVar(nextTypeVar)), nextTypeVar + 1)
      }
      | Name(name, _) => (Name(name, TypeVar(nextTypeVar)), nextTypeVar + 1)
      | If(cond, ifTrue, ifFalse, _) => {
        let (typedCond, nextTypeVar) = annotateExpr(cond, nextTypeVar)
        let (typedIfTrue, nextTypeVar) = annotateExpr(ifTrue, nextTypeVar)
        let (typedIfFalse, nextTypeVar) = annotateExpr(ifFalse, nextTypeVar)
        (If(typedCond, typedIfTrue, typedIfFalse, TypeVar(nextTypeVar)), nextTypeVar + 1)
      }
      | Rel(left, rel, right, _) => {
        let (typedLeft, nextTypeVar) = annotateExpr(left, nextTypeVar)
        let (typedRight, nextTypeVar) = annotateExpr(right, nextTypeVar)
        (Rel(typedLeft, rel, typedRight, TypeVar(nextTypeVar)), nextTypeVar + 1)
      }
      | Application(fun, arg, _) => {
        let (typedFun, nextTypeVar) = annotateExpr(fun, nextTypeVar)
        let (typedArg, nextTypeVar) = annotateExpr(arg, nextTypeVar)
        (Application(typedFun, typedArg, TypeVar(nextTypeVar)), nextTypeVar + 1)
      }
      | Cons(head, tail, _) => {
        let (typedHead, nextTypeVar) = annotateExpr(head, nextTypeVar)
        let (typedTail, nextTypeVar) = annotateExpr(tail, nextTypeVar)
        (Cons(typedHead, typedTail, TypeVar(nextTypeVar)), nextTypeVar + 1)
      }
    }
  }
  switch ast {
    | Let(name, isRec, params, body, _) => {
      let (typedBody, nextTypeVar) = annotateExpr(body, nextTypeVar)
      (Let(name, isRec, params, typedBody, TypeVar(nextTypeVar)), nextTypeVar + 1)
    }
  }
}

let rec substitute : (typeType, constraints) => typeType = (ty, constraints) => {
  switch ty {
    | TypeVar(v) => constraints->Belt.Map.Int.getWithDefault(v, ty)
    | TypeFun(t1, t2) => TypeFun(t1->substitute(constraints), t2->substitute(constraints))
    | TypePair(t1, t2) => TypePair(t1->substitute(constraints), t2->substitute(constraints))
    | TypeList(t) => TypeList(t->substitute(constraints))
    | _ => ty
  }
}

type typeInferer<'a> = TracedState.tracedState<inferenceState, 'a>

let getContext : typeInferer<context> = TracedState.get->TracedState.map(state => state.context)

let setContext : context => typeInferer<()> = context => TracedState.modify(state => {...state, context})

let withBindingContext : typeInferer<'a> => typeInferer<'a> = inferer => {
  getContext->TracedState.flatMap(context => {
    inferer->TracedState.flatMap(val => {
      setContext(context)->TracedState.map(_ => val)
    })
  })
}

let stepInfer : typeInferer<()> = {
  open AST

  let getConstraints : typeInferer<constraints> = TracedState.get->TracedState.map(state => state.constraints)

  let setConstraints : constraints => typeInferer<()> = constraints => TracedState.modify(state => {...state, constraints})

  let getAST : typeInferer<ast<typeType>> = TracedState.get->TracedState.map(state => state.typedAST)

  let getNextTypeVar : typeInferer<typeVar> = TracedState.get->TracedState.map(state => state.nextTypeVar)

  let setNextTypeVar : typeVar => typeInferer<()> = nextTypeVar => TracedState.modify(state => {...state, nextTypeVar})

  // QUESTION: Use the token in the context for error locations?
  let bind : (string, typeType) => typeInferer<()> = (name, ty) => {
    getContext->TracedState.flatMap(context => {
      // TODO: Check the name isn't bound yet. Look up OCaml behavior.
      setContext(context->Belt.Map.String.set(name, ty))
    })
  }

  exception UnificationError(typeType, typeType)

  let rec unify : (typeType, typeType) => typeInferer<()> = (t1, t2) => {
    getConstraints->TracedState.flatMap(constraints => {
      let t1 = t1->substitute(constraints)
      let t2 = t2->substitute(constraints)
      switch (t1, t2) {
        | (TypeVar(v), TypeVar(w)) if v == w =>
          TracedState.return(())
        | (TypeVar(v), _) => {
          // FIXME: Occurs check
          let constraints = constraints->Belt.Map.Int.map(ty => ty->substitute(Belt.Map.Int.fromArray([(v, t2)])))
          let constraints = constraints->Belt.Map.Int.set(v, t2)
          // TODO: Substitute types in ast?
          setConstraints(constraints)
        }
        | (_, TypeVar(_)) => unify(t2, t1)
        | (TypeInt, TypeInt)
        | (TypeFloat, TypeFloat)
        | (TypeString, TypeString)
        | (TypeBool, TypeBool) => TracedState.return(())
        | (TypeFun(a1, r1), TypeFun(a2, r2))
        | (TypePair(a1, r1), TypePair(a2, r2)) => {
          unify(a1, a2)->TracedState.flatMap(_ => unify(r1, r2))
        }
        | (TypeList(a), TypeList(b)) => {
          unify(a, b)
        }
        // TODO: Error handling
        | _ => raise(UnificationError(t1, t2))
      }
    })
  }

  let freshTypeVar : typeInferer<typeVar> = getNextTypeVar->TracedState.flatMap(v =>
    setNextTypeVar(v + 1)->TracedState.map(_ => v)
  )

  let rec stepInferPattern : pattern => typeInferer<typeType> = pattern => {
    switch pattern {
      | PairPat(first, second) => {
        stepInferPattern(first)->TracedState.flatMap(firstType => {
          stepInferPattern(second)->TracedState.flatMap(secondType => {
            TracedState.mark->TracedState.map(_ => TypePair(firstType, secondType))
          })
        })
      }
      | Wildcard => freshTypeVar->TracedState.map(v => TypeVar(v))
      | ListPat(patterns) => {
        freshTypeVar->TracedState.flatMap(v => {
          let elementTypeVar = TypeVar(v)
          patterns->Js.Array2.reduce((ma, pattern) => {
            ma->TracedState.flatMap(_ => {
              stepInferPattern(pattern)->TracedState.flatMap(patternType => {
                unify(patternType, elementTypeVar)
              })
            })
          }, TracedState.return(()))->TracedState.map(_ => TypeList(elementTypeVar))
        })
      }
      | ConsPat(head, tail) => {
        stepInferPattern(head)->TracedState.flatMap(headType => {
          stepInferPattern(tail)->TracedState.flatMap(tailType => {
            unify(TypeList(headType), tailType)->TracedState.map(_ =>
              tailType
            )
          })
        })
      }
      | NamePat(name) => {
        freshTypeVar->TracedState.flatMap(v => {
          let nameType = TypeVar(v)
          bind(name.lexeme, nameType)->TracedState.map(_ => nameType)
        })
      }
    }
  }

  let rec stepInferExpr : expr<typeType> => typeInferer<(typeType, expr<typeType>)> = expr => {
    switch expr {
      | Match(scrutinee, clauses, ty) => {
        stepInferExpr(scrutinee)->TracedState.flatMap(((scrutineeType, scrutinee)) => {
          clauses->Js.Array2.reduce((ma, (pattern, body)) => {
            ma->TracedState.flatMap(prevClauses => withBindingContext(
              stepInferPattern(pattern)->TracedState.flatMap(patternType => {
                unify(patternType, scrutineeType)->TracedState.flatMap(_ => {
                  stepInferExpr(body)->TracedState.flatMap(((bodyType, body)) => {
                    unify(bodyType, ty)->TracedState.map(_ => prevClauses->Js.Array2.concat([(pattern, body)]))
                  })
                })
              })
            ))
          }, TracedState.return([]))->TracedState.flatMap(clauses => {
            TracedState.mark->TracedState.map(_ => (ty, Match(scrutinee, clauses, ty)))
          })
        })
      }
      | Pair(first, second, ty) => {
        stepInferExpr(first)->TracedState.flatMap(((firstType, typedFirst)) => {
          stepInferExpr(second)->TracedState.flatMap(((secondType, typedSecond)) => {
            let pairType = TypePair(firstType, secondType)
            unify(pairType, ty)->TracedState.flatMap(_ => TracedState.mark)->TracedState.map(_ => (pairType, Pair(typedFirst, typedSecond, pairType)))
          })
        })
      }
      | Name(name, ty) => {
        getContext->TracedState.flatMap(context => {
          // TODO: Proper error handling
          let nameType = context->Belt.Map.String.getExn(name.lexeme)
          unify(nameType, ty)->TracedState.flatMap(_ => TracedState.mark)->TracedState.map(_ => (nameType, Name(name, nameType)))
        })
      }
      | If(condition, consequent, alternative, ty) => {
        stepInferExpr(condition)->TracedState.flatMap(((conditionType, typedCondition)) => {
          unify(conditionType, TypeBool)->TracedState.flatMap(_ => {
            stepInferExpr(consequent)->TracedState.flatMap(((consequentType, typedConsequent)) => {
              unify(consequentType, ty)->TracedState.flatMap(_ => {
                stepInferExpr(alternative)->TracedState.flatMap(((alternativeType, typedAlternative)) => {
                  unify(alternativeType, ty)->TracedState.flatMap(_ => TracedState.mark)->TracedState.map(_ => {
                    (consequentType, If(typedCondition, typedConsequent, typedAlternative, consequentType))
                  })
                })
              })
            })
          })
        })
      }
      | Rel(left, rel, right, ty) => {
        stepInferExpr(left)->TracedState.flatMap(((leftType, typedLeft)) => {
          stepInferExpr(right)->TracedState.flatMap(((rightType, typedRight)) => {
            switch rel {
              | GT => {
                unify(leftType, TypeInt)->TracedState.flatMap(_ => {
                  unify(rightType, TypeInt)
                })
              }
            }->TracedState.flatMap(_ => {
              unify(ty, TypeBool)
            })->TracedState.flatMap(_ => {
              TracedState.mark
            })->TracedState.map(_ => {
              (TypeBool, Rel(typedLeft, rel, typedRight, TypeBool))
            })
          })
        })
      }
      | Application(fun, arg, ty) => {
        stepInferExpr(fun)->TracedState.flatMap(((funType, typedFun)) => {
          stepInferExpr(arg)->TracedState.flatMap(((argType, typedArg)) => {
            unify(funType, TypeFun(argType, ty))->TracedState.flatMap(_ =>
              TracedState.mark
            )->TracedState.map(_ =>
              (ty, Application(typedFun, typedArg, ty))
            )
          })
        })
      }
      | Cons(head, tail, ty) => {
        stepInferExpr(head)->TracedState.flatMap(((headType, typedHead)) => {
          unify(TypeList(headType), ty)->TracedState.flatMap(_ =>
            stepInferExpr(tail)
          )->TracedState.flatMap(((tailType, typedTail)) =>
            unify(tailType, ty)->TracedState.flatMap(_ =>
              TracedState.mark
            )->TracedState.map(_ =>
              (tailType, Cons(typedHead, typedTail, tailType))
            )
          )
        })
      }
    }
  }

  getAST->TracedState.flatMap(ast => {
    switch ast {
      | Let(name, isRec, params, body, ty) => {
        let buildContext = params->Js.Array2.reduce((contextBuilder, param) => {
          contextBuilder->TracedState.flatMap(_ => {
            // TODO: Annotate params with types
            freshTypeVar->TracedState.flatMap(v => bind(param.lexeme, TypeVar(v)))
          })
        }, TracedState.return(()))->TracedState.flatMap(_ => {
          if isRec {
            bind(name.lexeme, ty)
          } else {
            TracedState.return(())
          }
        })
        withBindingContext(
          buildContext->TracedState.flatMap(_ =>
            stepInferExpr(body)->TracedState.flatMap(_ => {
              TracedState.mark
            })
          )
        )
      }
    }
  })
}

let makeInferenceState : AST.ast<'ann> => inferenceState = ast => {
  let (typedAST, nextTypeVar) = annotateWithTypeVars(ast, 0)
  {constraints: Belt.Map.Int.empty, nextTypeVar, typedAST, context: Belt.Map.String.empty}
}

let generateInferenceSteps : AST.ast<'ann> => TracedState.traceStep<inferenceState, ()> = ast => {
  let initialState = makeInferenceState(ast)
  stepInfer->TracedState.run(initialState)
}
