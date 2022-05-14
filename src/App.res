%%raw(`import './App.css';`)

@bs.module("./logo.svg") external logo: string = "default"

type tokenizerState = {
  index: int
}

type tokenType = WS | Keyword | ID | Symbol

type token = {
  type_: tokenType,
  lexeme: string
}

let printToken : token => string = t => `"${t.lexeme}"`

let printTokens : array<token> => string = ts => "[" ++ ts->Js.Array2.map(printToken)->Js.Array2.joinWith(", ") ++ "]"

let advance : (tokenizerState, int) => tokenizerState = (state, incr) => {
  {...state, index: state.index + incr}
}

let rec nextToken : (string, tokenizerState) => option<(token, tokenizerState)> = (string, state) => {
  let ws = %re("/\s+/y")
  Js.Re.setLastIndex(ws, state.index)
  let wsResult = Js.Re.exec_(ws, string)
  let letre = %re("/let|rec|match|if|then|else|with/y")
  Js.Re.setLastIndex(letre, state.index)
  let letResult = Js.Re.exec_(letre, string)
  let id = %re("/\w+/y")
  Js.Re.setLastIndex(id, state.index)
  let idResult = Js.Re.exec_(id, string)
  let symbol = %re("/[=(),|\[\]><_]|->|::/y")
  Js.Re.setLastIndex(symbol, state.index)
  let symbolResult = Js.Re.exec_(symbol, string)
  switch wsResult {
    | Some(wsResult) => {
      Js.Option.andThen((. match) => {
        nextToken(string, state->advance(Js.String.length(match)))

      }, Js.toOption(Js.Re.captures(wsResult)[0]))
    }
    | None => None
  }->Js.Option.firstSome(Js.Option.andThen((. result) => {
    Js.Option.map((. match) => {
      ({type_: Keyword, lexeme: match}, state->advance(Js.String.length(match)))
    }, Js.toOption(Js.Re.captures(result)[0]))
  }, letResult))->Js.Option.firstSome(Js.Option.andThen((. result) => {
    Js.Option.map((. match) => {
      ({type_: Symbol, lexeme: match}, state->advance(Js.String.length(match)))
    }, Js.toOption(Js.Re.captures(result)[0]))
  }, symbolResult))->Js.Option.firstSome(Js.Option.andThen((. result) => {
    Js.Option.map((. match) => {
      ({type_: ID, lexeme: match}, state->advance(Js.String.length(match)))
    }, Js.toOption(Js.Re.captures(result)[0]))
  }, idResult))
}

let tokenize : string => option<array<token>> = string => {
  let rec tokenize' : (string, tokenizerState) => option<array<token>> = (string, state) => {
    Js.log(`index: ${Belt.Int.toString(state.index)}`)
    switch nextToken(string, state) {
      | Some((token, state)) => Js.Option.map((. others) => {
        [token]->Js.Array2.concat(others)
      }, tokenize'(string, state))
      | None => if state.index >= Js.String.length(string) {
        Some([])
      } else { None }
    }
  }
  tokenize'(string, {index: 0})
}

type parserState = {
  index: int
}

type rec pattern = PairPat(pattern, pattern) | ListPat(array<pattern>) | Wildcard | ConsPat(pattern, pattern) | NamePat(token)

let rec printPattern : pattern => string = e => {
  switch e {
    | PairPat(first, second) => `PairPat(${printPattern(first)}, ${printPattern(second)})`
    | ListPat(patterns) => `ListPat(${patterns->Js.Array2.map(printPattern)->Js.Array2.joinWith(", ")})`
    | Wildcard => "Wildcard"
    | ConsPat(head, tail) => `ConsPat(${printPattern(head)}, ${printPattern(tail)})`
    | NamePat(name) => `NamePat(${printToken(name)})`
  }
}

type rel = GT

let printRel : rel => string = _ => "GT"

type rec clause = (pattern, expr)
and expr = Match(expr, array<clause>) | Pair(expr, expr) | Name(token) | If(expr, expr, expr) | Rel(expr, rel, expr) | Application(expr, expr) | Cons(expr, expr)

let rec printClause : clause => string = ((pattern, result)) => {
  `(${printPattern(pattern)}, ${printExpr(result)})`
}
and let printClauses : array<clause> => string = clauses => {
  clauses->Js.Array2.map(printClause)->Js.Array2.joinWith(", ")
}
and let printExpr : expr => string = e => {
  switch e {
    | Match(scrutinee, clauses) => `Match(${printExpr(scrutinee)}, ${printClauses(clauses)})`
    | Pair(first, second) => `Pair(${printExpr(first)}, ${printExpr(second)})`
    | Name(name) => `Name(${printToken(name)})`
    | If(test, ifTrue, ifFalse) => `If(${printExpr(test)}, ${printExpr(ifTrue)}, ${printExpr(ifFalse)})`
    | Rel(left, op, right) => `Rel(${printExpr(left)}, ${printRel(op)}, ${printExpr(right)})`
    | Application(fun, arg) => `Application(${printExpr(fun)}, ${printExpr(arg)})`
    | Cons(head, tail) => `Cons(${printExpr(head)}, ${printExpr(tail)})`
  }
}

type ast = Let(token, bool, array<token>, expr)

let printLet : ast => string = ast => {
  switch ast {
    | Let(name, isRec, params, rhs) => `Let(${printToken(name)}, ${if isRec { "true" } else { "false" }}, ${printTokens(params)}, ${printExpr(rhs)})`
  }
}

let parserAdvance : (parserState, int) => parserState = (state, incr) => {
  {...state, index: state.index + incr}
}

type parser<'a> = (array<token>, parserState) => result<('a, parserState), (string, parserState)>

let parseKeyword : string => parser<token> = (keyword, tokens, state) => {
  if Js.Array.length(tokens) <= state.index {
    Error(`keyword ${keyword}`, state)
  } else {
    switch tokens[state.index] {
      | {type_: Keyword, lexeme: k} => if (k == keyword) {
        Ok((tokens[state.index], state->parserAdvance(1)))
      } else { Error(`keyword ${keyword}`, state) }
      | _ => Error(`keyword ${keyword}`, state)
    }
  }
}

let optional : parser<'a> => parser<option<'a>> =
  (parser, tokens, state) => {
    parser(tokens, state)->Belt.Result.mapWithDefault(Ok(None, state), ((res, state)) => Ok(Some(res), state))
  }

let parseID : parser<token> = (tokens, state) => {
  if Js.Array.length(tokens) <= state.index {
    Error("id", state)
  } else {
    switch tokens[state.index] {
      | {type_: ID, lexeme: _} => Ok((tokens[state.index], state->parserAdvance(1)))
      | _ => Error("id", state)
    }
  }
}

let map : parser<'a> => ('a => 'b) => parser<'b> = (parser, f) => {
  (tokens, state) => {
    parser(tokens, state)->Belt.Result.map(((res, state)) => (f(res), state))
  }
}

let return : parser<'a> => 'b => parser<'b> = (parser, val) => parser->map(_ => val)

let many : parser<'a> => parser<array<'a>> = (parser, tokens, state) => {
  let results = []
  let state = ref(state)
  while parser(tokens, state.contents)->Belt.Result.isOk {
    let (r, s) = parser(tokens, state.contents)->Belt.Result.getExn
    results->Js.Array2.push(r)->ignore
    state := s
  }
  Ok((results, state.contents))
}

let parseNotEOF : parser<()> = (tokens, state) => {
  if Js.Array.length(tokens) <= state.index {
    Error("not end of file", state)
  } else {
    Ok(((), state))
  }
}

let parseSymbol : string => parser<token> = (symbol, tokens, state) => {
  parseNotEOF(tokens, state)->Belt.Result.flatMap((_) => {
    switch tokens[state.index] {
      | {type_: Symbol, lexeme: s} => if (s == symbol) {
        Ok((tokens[state.index], state->parserAdvance(1)))
      } else { Error(`symbol ${symbol}`, state) }
      | _ => Error(`symbol ${symbol}`, state)
    }
  })
}



let parserAnd : parser<'a> => parser<'b> => parser<('a, 'b)> = (first, second) => {
  (tokens, state) => {
    first(tokens, state)->Belt.Result.flatMap(((firstRes, state)) => {
      second->map((secondRes) => (firstRes, secondRes), tokens, state)
    })
  }
}

let seq : array<parser<'a>> => parser<array<'a>> = (parsers, tokens, state) => {
  parsers->Js.Array2.reduce((res, parser) => {
    res->Belt.Result.flatMap(((results, state)) => {
      parser(tokens, state)->Belt.Result.flatMap(((r, state)) => Ok(results->Js.Array2.concat([r]), state))
    })
  }, Ok([], state))
}

let skip : parser<'a> => parser<'b> => parser<'a> = (first, second) => {
  first->parserAnd(second)->map(((firstRes, _)) => firstRes)
}

let then : parser<'a> => parser<'b> => parser<'b> = (first, second) => {
  first->parserAnd(second)->map(((_, secondRes)) => secondRes)
}

let failWhen : parser<'a> => ('a => bool) => parser<'a> = (parser, predicate) => {
  (tokens, state) => {
    parser(tokens, state)->Belt.Result.flatMap(((res, state)) => {
      if (predicate(res)) {
        Error("predicate failed", state)
      } else {
        Ok((res, state))
      }
    })
  }
}

let sepBy : parser<'a> => parser<'b> => ~min: int=? => parser<array<'a>> = (parser, sep, ~min=0) => {
  parser->optional->map(first => first->Belt.Option.mapWithDefault([], f => [f]))->parserAnd(
    sep->then(parser)->many
  )->map(((first, rest)) => first->Js.Array2.concat(rest))->failWhen(
    list => list->Js.Array.length < min
  )
}

let rec makePairPattern : array<pattern> => result<pattern, string> = patterns => {
  switch patterns->Js.Array.length {
  | 0 | 1 => Error("at least 2 elements")
  | 2 => Ok(PairPat(patterns[0], patterns[1]))
  | _ => makePairPattern(Js.Array.sliceFrom(1, patterns))->Belt.Result.map(rest => PairPat(patterns[0], rest))
  }
}

let last : array<'a> => 'a = array => array[array->Js.Array2.length - 1]

let makePairs : array<expr> => result<expr, string> = exprs => {
  switch exprs->Js.Array.length {
  | 0 | 1 => Error("at least 2 elements")
  // | 2 => Ok(Pair(exprs[0], exprs[1]))
  // | _ => makePairs(Js.Array.sliceFrom(1, exprs))->Belt.Result.map(rest => Pair(exprs[0], rest))
  | _ => Ok(exprs->Js.Array2.slice(~start=0, ~end_=-1)->Js.Array2.reduceRight((rest, el) => Pair(el, rest), exprs->last))
  }
}

let mapError : result<'a, 'b> => ('b => 'c) => result<'a, 'c> = (result, f) => {
  switch result {
  | Error(b) => Error(f(b))
  | Ok(a) => Ok(a)
  }
}

let fallibleMap : parser<'a> => ('a => result<'b, string>) => parser<'b> = (parser, f, tokens, state) => {
  parser(tokens, state)->Belt.Result.flatMap(((res, state)) => {
    f(res)->Belt.Result.map(fres => (fres, state))->mapError(e => (e, state))
  })
}

let or : parser<'a> => parser<'a> => parser<'a> = (first, second, tokens, state) => {
  switch first(tokens, state) {
    | Ok(res) => Ok(res)
    | Error(err) => second(tokens, state)
  }
}

let alt : array<parser<'a>> => parser<'a> = (alternatives, tokens, state) => {
  alternatives->Belt.Array.reduce(Error("", state), (res, parser) => switch res {
    | Ok(res) => Ok(res)
    | Error(_) => parser(tokens, state)
  })
}

let parseWildcard : parser<pattern> = parseSymbol("_")->return(Wildcard)

let wrapLazy : Lazy.t<parser<'a>> => parser<'a> = (parser, tokens, state) => {
  Lazy.force(parser, tokens, state)
}

let trace : parser<'a> => string => parser<'a> = (parser, msg, tokens, state) => {
  let res = parser(tokens, state)
  switch res {
    | Ok((_, state)) => {
      Js.log(`${msg} success at ${state.index->Belt.Int.toString}`)
    }
    | Error(_, state) => {
      Js.log(`${msg} failure at ${state.index->Belt.Int.toString}`)
    }
  }
  res
}

let rec parsePairPattern : Lazy.t<parser<pattern>> =
  lazy (
    parseSymbol("(")
    ->then(parsePattern->wrapLazy->sepBy(parseSymbol(","), ~min=2))
    ->skip(parseSymbol(")"))
    ->fallibleMap(makePairPattern)
    ->trace("pair pattern")
  )
and let parseListPattern : Lazy.t<parser<pattern>> =
  lazy (
    parseSymbol("[")
    ->then(parsePattern->wrapLazy->sepBy(parseSymbol(",")))
    ->skip(parseSymbol("]"))
    ->map(ps => ListPat(ps))
    ->trace("list pattern")
  )
and let parseApplicationPattern : Lazy.t<parser<pattern>> = lazy (
  parseSymbol("(")->then(parsePattern->wrapLazy)->skip(parseSymbol(")"))
  ->or(parseWildcard)
  ->or(parseNamePattern->wrapLazy)
  ->or(parsePairPattern->wrapLazy)
  ->or(parseListPattern->wrapLazy)
)
and let parseConsPattern : Lazy.t<parser<pattern>> = lazy (
  parseApplicationPattern->wrapLazy
  ->skip(parseSymbol("::"))
  ->parserAnd(parsePattern->wrapLazy)
  ->map(((head, tail)) => ConsPat(head, tail))
  ->trace("cons pattern")
)
and let parseNamePattern : Lazy.t<parser<pattern>> = lazy (
  parseID->map(token => NamePat(token))
)
and let parsePattern : Lazy.t<parser<pattern>> =
  lazy (
    parseConsPattern->wrapLazy
    ->or(parseApplicationPattern->wrapLazy)
    ->trace("pattern")
  )


let rec parseClause : Lazy.t<parser<clause>> = lazy (
  parseSymbol("|")->then(parsePattern->wrapLazy)->skip(parseSymbol("->"))->parserAnd(parseExpr->wrapLazy))
and let parseMatch : Lazy.t<parser<expr>> =
  lazy (
    parseKeyword("match")
    ->then(parseExpr->wrapLazy)
    ->skip(parseKeyword("with"))
    ->parserAnd(parseClause->wrapLazy->many)
    ->map(((scrutinee, clauses)) => {
      Match(scrutinee, clauses)
    })
    ->or(parseIf->wrapLazy)
  )
and let parseIf : Lazy.t<parser<expr>> = lazy (
  parseKeyword("if")
  ->then(parseExpr->wrapLazy)
  ->skip(parseKeyword("then"))
  ->parserAnd(parseExpr->wrapLazy)
  ->skip(parseKeyword("else"))
  ->parserAnd(parseExpr->wrapLazy)
  ->map((((test, ifTrue), ifFalse)) => If(test, ifTrue, ifFalse))
  ->trace("if")
  ->or(parseConjunction->wrapLazy)
)
// & &&
and let parseConjunction : Lazy.t<parser<expr>> = lazy (
  parseRelational->wrapLazy
)
// =… <… >… |… &… $… !=
and let parseRelational : Lazy.t<parser<expr>> = lazy (
  parseCons->wrapLazy
  ->parserAnd(parseSymbol(">")->return(GT)->parserAnd(parseCons->wrapLazy)->many)
  ->map(((first, opPairs)) => opPairs->Belt.Array.reduce(first, (expr, (op, operand)) => Rel(expr, op, operand)))
  ->trace("relational")
)
and let parseNegation : Lazy.t<parser<expr>> = lazy (
  parseApplication->wrapLazy
)
and let parseApplication : Lazy.t<parser<expr>> = lazy (
  parsePrimary->wrapLazy
  ->parserAnd(parsePrimary->wrapLazy->many)
  ->map(((fun, args)) => args->Belt.Array.reduce(fun, (expr, arg) => Application(expr, arg)))
)
and let parsePrimary : Lazy.t<parser<expr>> = lazy (
  alt([
    parseSymbol("(")
    ->then(parseExpr->wrapLazy->sepBy(parseSymbol(",")))
    ->fallibleMap(exprs =>
      switch exprs->Js.Array.length {
        | 1 => Ok(exprs[0])
        | _ => makePairs(exprs)
      }
    )->skip(parseSymbol(")")),
    parseID->map(id => Name(id)),
  ])
)
// ::
and let parseCons : Lazy.t<parser<expr>> = lazy (
  parseNegation->wrapLazy
  ->skip(parseSymbol("::"))
  ->parserAnd(parseCons->wrapLazy)
  ->map(((head, tail)) => Cons(head, tail))
  ->or(parseNegation->wrapLazy)
)
and let parseExpr : Lazy.t<parser<expr>> = lazy (alt([
  parseMatch->wrapLazy,
  (parseIf->wrapLazy),
  (parseRelational->wrapLazy),
  (parseApplication->wrapLazy)
])->trace("expression"))

let parseLet : parser<ast> = {
  parseKeyword("let")
    ->then(parseKeyword("rec")->optional)
    ->parserAnd(parseID)
    ->parserAnd(parseID->many)
    ->skip(parseSymbol("="))
    ->parserAnd(parseExpr->wrapLazy)
    ->map(((((recToken, name), params), rhs)) => {
      Let(name, recToken->Belt.Option.isSome, params, rhs)
    })
}

let fail : parser<'a> => ~msg:string=? => parser<()> = (parser, ~msg="error", tokens, state) => switch parser(tokens, state) {
  | Ok(_) => Error(msg, state)
  | Error(_) => Ok((), state)
}

let parseEOF : parser<()> = parseNotEOF->fail(~msg="end of file")

let parse : array<token> => result<ast, (string, parserState)> = tokens => {
  parseLet->skip(parseEOF, tokens, {index: 0})->Belt.Result.map(((ast, _)) => ast)
}

@react.component
let make = () => {
  let (codeInput, setCodeInput) = React.useState(_ => "")

  let onCodeInputChange = event => {
    ReactEvent.Form.preventDefault(event)
    let value = ReactEvent.Form.target(event)["value"]
    setCodeInput(_ => value)
  }

  let onSubmitCodeInput = event => {
    ReactEvent.Form.preventDefault(event)
    Js.log(codeInput)
    let tokens = tokenize(codeInput)
    Js.log(tokens)

    tokens->Belt.Option.map(tokens => {
      let ast = parse(tokens)
      switch ast {
        | Ok(ast) => {
          Js.log(ast)
          Js.log(printLet(ast))
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
  </div>
}
