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

let advance : (tokenizerState, int) => tokenizerState = (state, incr) => {
  {...state, index: state.index + incr}
}

let rec nextToken : (string, tokenizerState) => option<(token, tokenizerState)> = (string, state) => {
  let ws = %re("/\s+/y")
  Js.Re.setLastIndex(ws, state.index)
  let wsResult = Js.Re.exec_(ws, string)
  let letre = %re("/let|rec|match|if|then|else/y")
  Js.Re.setLastIndex(letre, state.index)
  let letResult = Js.Re.exec_(letre, string)
  let id = %re("/\w+/y")
  Js.Re.setLastIndex(id, state.index)
  let idResult = Js.Re.exec_(id, string)
  let symbol = %re("/[=(),|\[\]><]|->|::/y")
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
      ({type_: ID, lexeme: match}, state->advance(Js.String.length(match)))
    }, Js.toOption(Js.Re.captures(result)[0]))
  }, idResult))->Js.Option.firstSome(Js.Option.andThen((. result) => {
    Js.Option.map((. match) => {
      ({type_: Symbol, lexeme: match}, state->advance(Js.String.length(match)))
    }, Js.toOption(Js.Re.captures(result)[0]))
  }, symbolResult))
}

let tokenize : string => option<array<token>> = string => {
  let rec tokenize' : (string, tokenizerState) => option<array<token>> = (string, state) => {
    Js.log(`index: ${Belt.Int.toString(state.index)}`)
    switch nextToken(string, state) {
      | Some((token, state)) => Js.Option.map((. others) => {
        Js.Array.concat(others, [token])
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

type rec pattern = PairPat(pattern, pattern) | ListPat(array<pattern>) | Wildcard | ConsPat(pattern, pattern)

type rel = GT

type rec clause = (pattern, expr)
and expr = Match(expr, array<clause>) | Pair(expr, expr) | Name(token) | If(expr, expr, expr) | Rel(expr, rel, expr) | Application(expr, expr)

type ast = Let(token, bool, array<token>, expr)

let parserAdvance : (parserState, int) => parserState = (state, incr) => {
  {...state, index: state.index + incr}
}

type parser<'a> = (array<token>, parserState) => option<('a, parserState)>

let parseKeyword : (string, array<token>, parserState) => option<(token, parserState)> = (keyword, tokens, state) => {
  if Js.Array.length(tokens) <= state.index {
    None
  } else {
    switch tokens[state.index] {
      | {type_: Keyword, lexeme: k} => if (k == keyword) {
        Some((tokens[state.index], state->parserAdvance(1)))
      } else { None }
      | _ => None
    }
  }
}

let optional : parser<'a> => parser<option<'a>> =
  (parser) => {
    (tokens, state) => {
      parser(tokens, state)->Belt.Option.mapWithDefault(Some(None, state), ((res, state)) => Some(Some(res), state))
    }
  }

let parseID : parser<token> = (tokens, state) => {
  if Js.Array.length(tokens) <= state.index {
    None
  } else {
    switch tokens[state.index] {
      | {type_: ID, lexeme: _} => Some((tokens[state.index], state->parserAdvance(1)))
      | _ => None
    }
  }
}

let map : parser<'a> => ('a => 'b) => parser<'b> = (parser, f) => {
  (tokens, state) => {
    parser(tokens, state)->Belt.Option.map(((res, state)) => (f(res), state))
  }
}

let return : parser<'a> => 'b => parser<'b> = (parser, val) => parser->map(_ => val)

// let rec many : parser<'a> => parser<array<'a>> = (parser) => {
//   let cons = (res: 'a, rest: array<'a>) => [res]->Js.Array.concat(rest)
//   (tokens, state) => {
//     switch parser(tokens, state) {
//       | Some((res, state)) => {
//         parser->many->map(cons(res), tokens, state)
//       }
//       | None => Some(([], state))
//     }
//   }
// }
let many : parser<'a> => parser<array<'a>> = (parser, tokens, state) => {
  let results = []
  let state = ref(state)
  while parser(tokens, state.contents)->Belt.Option.isSome {
    let (r, s) = parser(tokens, state.contents)->Belt.Option.getExn
    results->Js.Array2.push(r)->ignore
    state := s
  }
  Some((results, state.contents))
}

let parseNotEOF : parser<()> = (tokens, state) => {
  if Js.Array.length(tokens) <= state.index {
    None
  } else {
    Some(((), state))
  }
}

let parseSymbol : string => parser<token> = (symbol, tokens, state) => {
  parseNotEOF(tokens, state)->Belt.Option.flatMap((_) => {
    switch tokens[state.index] {
      | {type_: Symbol, lexeme: s} => if (s == symbol) {
        Some((tokens[state.index], state->parserAdvance(1)))
      } else { None }
      | _ => None
    }
  })
}



let parserAnd : parser<'a> => parser<'b> => parser<('a, 'b)> = (first, second) => {
  (tokens, state) => {
    first(tokens, state)->Belt.Option.flatMap(((firstRes, state)) => {
      second->map((secondRes) => (firstRes, secondRes), tokens, state)
    })
  }
}

let seq : array<parser<'a>> => parser<array<'a>> = (parsers, tokens, state) => {
  parsers->Js.Array2.reduce((res, parser) => {
    res->Belt.Option.flatMap(((results, state)) => {
      parser(tokens, state)->Belt.Option.flatMap(((r, state)) => Some(results->Js.Array.concat([r]), state))
    })
  }, Some([], state))
}

let skip : parser<'a> => parser<'b> => parser<'a> = (first, second) => {
  first->parserAnd(second)->map(((firstRes, _)) => firstRes)
}

let then : parser<'a> => parser<'b> => parser<'b> = (first, second) => {
  first->parserAnd(second)->map(((_, secondRes)) => secondRes)
}

let failWhen : parser<'a> => ('a => bool) => parser<'a> = (parser, predicate) => {
  (tokens, state) => {
    parser(tokens, state)->Belt.Option.flatMap(((res, state)) => {
      if (predicate(res)) {
        None
      } else {
        Some((res, state))
      }
    })
  }
}

let sepBy : parser<'a> => parser<'b> => ~min: int=? => parser<array<'a>> = (parser, sep, ~min=?) => {
  parser->optional->map(first => first->Belt.Option.mapWithDefault([], f => [f]))->parserAnd(
    sep->then(parser)->many
  )->map(((first, rest)) => first->Js.Array.concat(rest))->failWhen(
    list => min->Belt.Option.mapWithDefault(false, min => list->Js.Array.length < min)
  )
}

let rec makePairPattern : array<pattern> => option<pattern> = patterns => {
  switch patterns->Js.Array.length {
  | 0 | 1 => None
  | 2 => Some(PairPat(patterns[0], patterns[1]))
  | _ => makePairPattern(Js.Array.sliceFrom(1, patterns))->Belt.Option.map(rest => PairPat(patterns[0], rest))
  }
}

let rec makePairs : array<expr> => option<expr> = exprs => {
  switch exprs->Js.Array.length {
  | 0 | 1 => None
  | 2 => Some(Pair(exprs[0], exprs[1]))
  | _ => makePairs(Js.Array.sliceFrom(1, exprs))->Belt.Option.map(rest => Pair(exprs[0], rest))
  }
}

let fallibleMap : parser<'a> => ('a => option<'b>) => parser<'b> = (parser, f) => {
  (tokens, state) => {
    parser(tokens, state)->Belt.Option.flatMap(((res, state)) => {
      f(res)->Belt.Option.map(fres => (fres, state))
    })
  }
}

let or : parser<'a> => parser<'a> => parser<'a> = (first, second, tokens, state) => {
  switch first(tokens, state) {
    | Some(res) => Some(res)
    | None => second(tokens, state)
  }
}

let alt : array<parser<'a>> => parser<'a> = (alternatives, tokens, state) => {
  alternatives->Belt.Array.reduce(None, (res, parser) => switch res {
    | Some(res) => Some(res)
    | None => parser(tokens, state)
  })
}

let parseWildcard : parser<pattern> = parseSymbol("_")->return(Wildcard)

let wrapLazy : Lazy.t<parser<'a>> => parser<'a> = (parser, tokens, state) => {
  Lazy.force(parser, tokens, state)
}

let rec parsePairPattern : Lazy.t<parser<pattern>> =
  lazy ((parseSymbol("(")->then(parsePattern->wrapLazy)->sepBy(parseSymbol(","), ~min=2))->skip(parseSymbol(")"))->fallibleMap(makePairPattern))
and let parseListPattern : Lazy.t<parser<pattern>> =
  lazy ((parseSymbol("[")
    ->then(parsePattern->wrapLazy)->sepBy(parseSymbol(",")))
    ->skip(parseSymbol("]"))
    ->map(ps => ListPat(ps)))
and let parseConsPattern : Lazy.t<parser<pattern>> = lazy (parsePattern->wrapLazy->skip(parseSymbol("::"))->parserAnd(parsePattern->wrapLazy)->map(((head, tail)) => ConsPat(head, tail)))
and let parsePattern : Lazy.t<parser<pattern>> =
  lazy (parsePairPattern->wrapLazy->or(parseListPattern->wrapLazy)->or(parseWildcard)->or(parseConsPattern->wrapLazy))


let rec parseClause : Lazy.t<parser<clause>> = lazy (parseSymbol("|")->then(parsePattern->wrapLazy)->skip(parseSymbol("->"))->parserAnd(parseExpr->wrapLazy))
and let parseMatch : Lazy.t<parser<expr>> =
  lazy (parseKeyword("match")
    ->then(parseExpr->wrapLazy)
    ->skip(parseKeyword("with"))
    ->parserAnd(parseClause->wrapLazy->many)
    ->map(((scrutinee, clauses)) => {
      Match(scrutinee, clauses)
    }))
and let parseIf : Lazy.t<parser<expr>> = lazy (parseKeyword("if")
  ->then(parseExpr->wrapLazy)
  ->skip(parseKeyword("then"))
  ->parserAnd(parseExpr->wrapLazy)
  ->skip(parseKeyword("else"))
  ->parserAnd(parseExpr->wrapLazy)
  ->map((((test, ifTrue), ifFalse)) => If(test, ifTrue, ifFalse)))
and let parseRelational : Lazy.t<parser<expr>> = lazy (parseExpr->wrapLazy
  ->parserAnd(parseSymbol(">")->return(GT))
  ->parserAnd(parseExpr->wrapLazy)
  ->map((((left, rel), right)) => Rel(left, rel, right)))
and let parseApplication : Lazy.t<parser<expr>> = lazy (parseExpr->wrapLazy
  ->parserAnd(parseExpr->wrapLazy)
  ->map(((fun, arg)) => Application(fun, arg)))
and let parseExpr : Lazy.t<parser<expr>> = lazy alt([
  parseMatch->wrapLazy,
  seq([
    parseSymbol("(")->map(s => Name(s)),
    (parseExpr->wrapLazy->sepBy(parseSymbol(",")))->fallibleMap(exprs =>
      switch exprs->Js.Array.length {
        | 1 => Some(exprs[0])
        | _ => makePairs(exprs)
      }),
    (parseSymbol(")"))->map(s => Name(s))])->map(parts => parts[2]),
  (parseID->map(id => Name(id))),
  (parseIf->wrapLazy),
  (parseRelational->wrapLazy),
  (parseApplication->wrapLazy)])

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

let fail : parser<'a> => parser<()> = (parser, tokens, state) => switch parser(tokens, state) {
  | Some(_) => None
  | None => Some((), state)
}

let parseEOF : parser<()> = parseNotEOF->fail

let parse : array<token> => option<ast> = tokens => {
  parseLet->skip(parseEOF, tokens, {index: 0})->Belt.Option.map(((ast, _)) => ast)
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
    let ast = tokens->Belt.Option.flatMap(tokens => parse(tokens))
    Js.log(ast)
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
