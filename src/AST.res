type tokenType = WS | Keyword | ID | Symbol

let tokenTypeToString = (ty: tokenType): string => {
  switch ty {
  | WS => "WS"
  | Keyword => "Keyword"
  | ID => "ID"
  | Symbol => "Symbol"
  }
}

type token = {
  type_: tokenType,
  lexeme: string,
}

let printToken: token => string = t => `"${t.lexeme}"`

let printTokens: array<token> => string = ts =>
  "[" ++ ts->Array.map(printToken)->Array.joinWith(", ") ++ "]"

type tokenizerState = {index: int}

let advance: (tokenizerState, int) => tokenizerState = (state, incr) => {
  {index: state.index + incr}
}

let rec nextToken: (string, tokenizerState) => option<(token, tokenizerState)> = (
  string,
  state,
) => {
  let ws = %re("/\s+/y")
  ws->RegExp.setLastIndex(state.index)
  let wsResult = ws->RegExp.exec(string)
  let letre = %re("/let|rec|match|if|then|else|with/y")
  letre->RegExp.setLastIndex(state.index)
  let letResult = letre->RegExp.exec(string)
  let id = %re("/\w+/y")
  id->RegExp.setLastIndex(state.index)
  let idResult = id->RegExp.exec(string)
  let symbol = %re("/[=(),|\[\]><_]|->|::/y")
  symbol->RegExp.setLastIndex(state.index)
  let symbolResult = symbol->RegExp.exec(string)
  switch wsResult {
  | Some(wsResult) => {
      let match = wsResult->RegExp.Result.fullMatch

      nextToken(string, state->advance(match->String.length))
    }
  | None => None
  }
  ->Option.orElse(
    letResult->Option.flatMap(result => {
      let match = result->RegExp.Result.fullMatch
      Some(({type_: Keyword, lexeme: match}, state->advance(match->String.length)))
    }),
  )
  ->Option.orElse(
    symbolResult->Option.flatMap(result => {
      let match = result->RegExp.Result.fullMatch
      Some(({type_: Symbol, lexeme: match}, state->advance(match->String.length)))
    }),
  )
  ->Option.orElse(
    idResult->Option.flatMap(result => {
      let match = result->RegExp.Result.fullMatch
      Some(({type_: ID, lexeme: match}, state->advance(match->String.length)))
    }),
  )
}

let tokenize: string => option<array<token>> = string => {
  let rec tokenize': (string, tokenizerState) => option<array<token>> = (string, state) => {
    Console.log(`index: ${Int.toString(state.index)}`)
    switch nextToken(string, state) {
    | Some((token, state)) =>
      tokenize'(string, state)->Option.map(others => {
        [token]->Array.concat(others)
      })
    | None =>
      if state.index >= string->String.length {
        Some([])
      } else {
        None
      }
    }
  }
  tokenize'(string, {index: 0})
}

type rel = GT

let printRel: rel => string = _ => "GT"
let relToFriendlyString = (_: rel): string => ">"

type rec pattern =
  | PairPat(pattern, pattern)
  | ListPat(array<pattern>)
  | Wildcard
  | ConsPat(pattern, pattern)
  | NamePat(token)

let rec printPattern: pattern => string = e => {
  switch e {
  | PairPat(first, second) => `PairPat(${printPattern(first)}, ${printPattern(second)})`
  | ListPat(patterns) => `ListPat(${patterns->Array.map(printPattern)->Array.joinWith(", ")})`
  | Wildcard => "Wildcard"
  | ConsPat(head, tail) => `ConsPat(${printPattern(head)}, ${printPattern(tail)})`
  | NamePat(name) => `NamePat(${printToken(name)})`
  }
}

type rec clause<'ann> = (pattern, expr<'ann>)
and expr<'ann> =
  | Match(expr<'ann>, array<clause<'ann>>, 'ann)
  | Pair(expr<'ann>, expr<'ann>, 'ann)
  | Name(token, 'ann)
  | If(expr<'ann>, expr<'ann>, expr<'ann>, 'ann)
  | Rel(expr<'ann>, rel, expr<'ann>, 'ann)
  | Application(expr<'ann>, expr<'ann>, 'ann)
  | Cons(expr<'ann>, expr<'ann>, 'ann)

let rec printClause: clause<'ann> => string = ((pattern, result)) => {
  `(${printPattern(pattern)}, ${printExpr(result)})`
}
and printClauses: array<clause<'ann>> => string = clauses => {
  clauses->Array.map(printClause)->Array.joinWith(", ")
}
and printExpr: expr<'ann> => string = e => {
  switch e {
  | Match(scrutinee, clauses, _) => `Match(${printExpr(scrutinee)}, ${printClauses(clauses)})`
  | Pair(first, second, _) => `Pair(${printExpr(first)}, ${printExpr(second)})`
  | Name(name, _) => `Name(${printToken(name)})`
  | If(test, ifTrue, ifFalse, _) =>
    `If(${printExpr(test)}, ${printExpr(ifTrue)}, ${printExpr(ifFalse)})`
  | Rel(left, op, right, _) => `Rel(${printExpr(left)}, ${printRel(op)}, ${printExpr(right)})`
  | Application(fun, arg, _) => `Application(${printExpr(fun)}, ${printExpr(arg)})`
  | Cons(head, tail, _) => `Cons(${printExpr(head)}, ${printExpr(tail)})`
  }
}

type ast<'ann> = Let(token, bool, array<token>, expr<'ann>, 'ann)

let printLet: ast<'ann> => string = ast => {
  switch ast {
  | Let(name, isRec, params, rhs, _) =>
    `Let(${printToken(name)}, ${if isRec {
        "true"
      } else {
        "false"
      }}, ${printTokens(params)}, ${printExpr(rhs)})`
  }
}

type parserState = {index: int}

type parser<'a> = (array<token>, parserState) => result<('a, parserState), (string, parserState)>

let parserAdvance: (parserState, int) => parserState = (state, incr) => {
  {index: state.index + incr}
}

let parseKeyword: string => parser<token> = keyword => (tokens, state) => {
  if tokens->Array.length <= state.index {
    Error(`keyword ${keyword}`, state)
  } else {
    let currentToken = tokens->Array.getUnsafe(state.index)
    switch currentToken {
    | {type_: Keyword, lexeme: k} =>
      if k == keyword {
        Ok(currentToken, state->parserAdvance(1))
      } else {
        Error(`keyword ${keyword}`, state)
      }
    | _ => Error(`keyword ${keyword}`, state)
    }
  }
}

let optional: parser<'a> => parser<option<'a>> = parser => (tokens, state) => {
  parser(tokens, state)->Result.mapWithDefault(Ok(None, state), ((res, state)) => Ok(
    Some(res),
    state,
  ))
}

let parseID: parser<token> = (tokens, state) => {
  if tokens->Array.length <= state.index {
    Error("id", state)
  } else {
    let currentToken = tokens->Array.getUnsafe(state.index)
    switch currentToken {
    | {type_: ID, lexeme: _} => Ok(currentToken, state->parserAdvance(1))
    | _ => Error("id", state)
    }
  }
}

let map: (parser<'a>, 'a => 'b) => parser<'b> = (parser, f) => {
  (tokens, state) => {
    parser(tokens, state)->Result.map(((res, state)) => (f(res), state))
  }
}

let return: (parser<'a>, 'b) => parser<'b> = (parser, val) => parser->map(_ => val)

let many: parser<'a> => parser<array<'a>> = parser => (tokens, state) => {
  let results = []
  let state = ref(state)
  while parser(tokens, state.contents)->Result.isOk {
    let (r, s) = parser(tokens, state.contents)->Result.getExn
    results->Array.push(r)->ignore
    state := s
  }
  Ok((results, state.contents))
}

let parseNotEOF: parser<unit> = (tokens, state) => {
  if tokens->Array.length <= state.index {
    Error("not end of file", state)
  } else {
    Ok(((), state))
  }
}

let parseSymbol: string => parser<token> = symbol => (tokens, state) => {
  parseNotEOF(tokens, state)->Result.flatMap(_ => {
    let currentToken = tokens->Array.getUnsafe(state.index)

    switch currentToken {
    | {type_: Symbol, lexeme: s} =>
      if s == symbol {
        Ok((currentToken, state->parserAdvance(1)))
      } else {
        Error(`symbol ${symbol}`, state)
      }
    | _ => Error(`symbol ${symbol}`, state)
    }
  })
}

let parserAnd: (parser<'a>, parser<'b>) => parser<('a, 'b)> = (first, second) => {
  (tokens, state) => {
    first(tokens, state)->Result.flatMap(((firstRes, state)) => {
      map(second, secondRes => (firstRes, secondRes))(tokens, state)
    })
  }
}

let or: (parser<'a>, parser<'a>) => parser<'a> = (first, second) => (tokens, state) => {
  switch first(tokens, state) {
  | Ok(res) => Ok(res)
  // TODO: Construct an error message that includes content from the first.
  | Error(_) => second(tokens, state)
  }
}

let alt: array<parser<'a>> => parser<'a> = alternatives => (tokens, state) => {
  alternatives->Array.reduce(Error("", state), (res, parser) =>
    switch res {
    | Ok(res) => Ok(res)
    | Error(_) => parser(tokens, state)
    }
  )
}

let seq: array<parser<'a>> => parser<array<'a>> = parsers => (tokens, state) => {
  parsers->Array.reduce(Ok([], state), (res, parser) => {
    res->Result.flatMap(((results, state)) => {
      parser(tokens, state)->Result.flatMap(((r, state)) => Ok(results->Array.concat([r]), state))
    })
  })
}

let skip: (parser<'a>, parser<'b>) => parser<'a> = (first, second) => {
  first->parserAnd(second)->map(fst)
}

let then: (parser<'a>, parser<'b>) => parser<'b> = (first, second) => {
  first->parserAnd(second)->map(snd)
}

let failWhen: (parser<'a>, 'a => bool) => parser<'a> = (parser, predicate) => {
  (tokens, state) => {
    parser(tokens, state)->Result.flatMap(((res, state)) => {
      if predicate(res) {
        Error("predicate failed", state)
      } else {
        Ok((res, state))
      }
    })
  }
}

let fail: (parser<'a>, ~msg: string=?) => parser<unit> = (parser, ~msg="error") => (
  tokens,
  state,
) =>
  switch parser(tokens, state) {
  | Ok(_) => Error(msg, state)
  | Error(_) => Ok((), state)
  }

let sepBy: (parser<'a>, parser<'b>, ~min: int=?) => parser<array<'a>> = (parser, sep, ~min=0) => {
  parser
  ->optional
  ->map(first => first->Option.mapOr([], f => [f]))
  ->parserAnd(sep->then(parser)->many)
  ->map(((first, rest)) => first->Array.concat(rest))
  ->failWhen(list => list->Array.length < min)
}

let wrapLazy: Lazy.t<parser<'a>> => parser<'a> = parser => (tokens, state) => {
  Lazy.force(parser)(tokens, state)
}

let trace: (parser<'a>, string) => parser<'a> = (parser, msg) => (tokens, state) => {
  let res = parser(tokens, state)
  switch res {
  | Ok((_, state)) => Console.log(`${msg} success at ${state.index->Int.toString}`)
  | Error(_, state) => Console.log(`${msg} failure at ${state.index->Int.toString}`)
  }
  res
}

let rec makePairPattern: array<pattern> => result<pattern, string> = patterns => {
  switch patterns {
  | [] | [_] => Error("at least 2 elements")
  | [first, second] => Ok(PairPat(first, second))
  | _ =>
    makePairPattern(patterns->Array.sliceToEnd(~start=1))->Result.map(rest => PairPat(
      patterns->Array.getUnsafe(0),
      rest,
    ))
  }
}

let makePairs: array<expr<unit>> => result<expr<unit>, string> = exprs => {
  open Array
  switch exprs->Array.length {
  | 0 | 1 => Error("at least 2 elements")
  | _ =>
    Ok(
      exprs
      ->Array.slice(~start=0, ~end=-1)
      ->Array.reduceRight(exprs->last->Option.getUnsafe, (rest, el) => Pair(el, rest, ())),
    )
  }
}

let fallibleMap: (parser<'a>, 'a => result<'b, string>) => parser<'b> = (parser, f) => (
  tokens,
  state,
) => {
  open Result
  parser(tokens, state)->Result.flatMap(((res, state)) => {
    f(res)->Result.map(fres => (fres, state))->mapError(e => (e, state))
  })
}

let parseWildcard: parser<pattern> = parseSymbol("_")->return(Wildcard)

let rec parsePairPattern: Lazy.t<parser<pattern>> = lazy (
  parseSymbol("(")
  ->then(parsePattern->wrapLazy->sepBy(parseSymbol(","), ~min=2))
  ->skip(parseSymbol(")"))
  ->fallibleMap(makePairPattern)
  ->trace("pair pattern")
)
and parseListPattern: Lazy.t<parser<pattern>> = lazy (
  parseSymbol("[")
  ->then(parsePattern->wrapLazy->sepBy(parseSymbol(","), ~min=0))
  ->skip(parseSymbol("]"))
  ->map(ps => ListPat(ps))
  ->trace("list pattern")
)
and parseApplicationPattern: Lazy.t<parser<pattern>> = lazy (
  parseSymbol("(")
  ->then(parsePattern->wrapLazy)
  ->skip(parseSymbol(")"))
  ->or(parseWildcard)
  ->or(parseNamePattern->wrapLazy)
  ->or(parsePairPattern->wrapLazy)
  ->or(parseListPattern->wrapLazy)
)
and parseConsPattern: Lazy.t<parser<pattern>> = lazy (
  parseApplicationPattern
  ->wrapLazy
  ->skip(parseSymbol("::"))
  ->parserAnd(parsePattern->wrapLazy)
  ->map(((head, tail)) => ConsPat(head, tail))
  ->trace("cons pattern")
)
and parseNamePattern: Lazy.t<parser<pattern>> = lazy (parseID->map(token => NamePat(token)))
and parsePattern: Lazy.t<parser<pattern>> = lazy (
  parseConsPattern
  ->wrapLazy
  ->or(parseApplicationPattern->wrapLazy)
  ->trace("pattern")
)

let rec parseClause: Lazy.t<parser<clause<'ann>>> = lazy (
  parseSymbol("|")
  ->then(parsePattern->wrapLazy)
  ->skip(parseSymbol("->"))
  ->parserAnd(parseExpr->wrapLazy)
)
and parseMatch: Lazy.t<parser<expr<unit>>> = lazy (
  parseKeyword("match")
  ->then(parseExpr->wrapLazy)
  ->skip(parseKeyword("with"))
  ->parserAnd(parseClause->wrapLazy->many)
  ->map(((scrutinee, clauses)) => {
    Match(scrutinee, clauses, ())
  })
  ->or(parseIf->wrapLazy)
)
and parseIf: Lazy.t<parser<expr<unit>>> = lazy (
  parseKeyword("if")
  ->then(parseExpr->wrapLazy)
  ->skip(parseKeyword("then"))
  ->parserAnd(parseExpr->wrapLazy)
  ->skip(parseKeyword("else"))
  ->parserAnd(parseExpr->wrapLazy)
  ->map((((test, ifTrue), ifFalse)) => If(test, ifTrue, ifFalse, ()))
  ->trace("if")
  ->or(parseConjunction->wrapLazy)
)
// & &&
and parseConjunction: Lazy.t<parser<expr<unit>>> = lazy (parseRelational->wrapLazy)
// =… <… >… |… &… $… !=
and parseRelational: Lazy.t<parser<expr<unit>>> = lazy (
  parseCons
  ->wrapLazy
  ->parserAnd(parseSymbol(">")->return(GT)->parserAnd(parseCons->wrapLazy)->many)
  ->map(((first, opPairs)) =>
    opPairs->Array.reduce(first, (expr, (op, operand)) => Rel(expr, op, operand, ()))
  )
  ->trace("relational")
)
and parseNegation: Lazy.t<parser<expr<unit>>> = lazy (parseApplication->wrapLazy)
and parseApplication: Lazy.t<parser<expr<unit>>> = lazy (
  parsePrimary
  ->wrapLazy
  ->parserAnd(parsePrimary->wrapLazy->many)
  ->map(((fun, args)) => args->Array.reduce(fun, (expr, arg) => Application(expr, arg, ())))
)
and parsePrimary: Lazy.t<parser<expr<unit>>> = lazy alt([
  parseSymbol("(")
  ->then(parseExpr->wrapLazy->sepBy(parseSymbol(","), ~min=1))
  ->fallibleMap(exprs =>
    switch exprs->Array.length {
    | 1 => Ok(exprs->Array.getUnsafe(0))
    | _ => makePairs(exprs)
    }
  )
  ->skip(parseSymbol(")")),
  parseID->map(id => Name(id, ())),
])
// ::
and parseCons: Lazy.t<parser<expr<unit>>> = lazy (
  parseNegation
  ->wrapLazy
  ->skip(parseSymbol("::"))
  ->parserAnd(parseCons->wrapLazy)
  ->map(((head, tail)) => Cons(head, tail, ()))
  ->or(parseNegation->wrapLazy)
)
and parseExpr: Lazy.t<parser<expr<'ann>>> = lazy (
  alt([
    parseMatch->wrapLazy,
    parseIf->wrapLazy,
    parseRelational->wrapLazy,
    parseApplication->wrapLazy,
  ])->trace("expression")
)

let parseLet: parser<ast<unit>> = {
  parseKeyword("let")
  ->then(parseKeyword("rec")->optional)
  ->parserAnd(parseID)
  ->parserAnd(parseID->many)
  ->skip(parseSymbol("="))
  ->parserAnd(parseExpr->wrapLazy)
  ->map(((((recToken, name), params), rhs)) => {
    Let(name, recToken->Option.isSome, params, rhs, ())
  })
}

let parseEOF: parser<unit> = parseNotEOF->fail(~msg="end of file")

let parse: array<token> => result<ast<unit>, (string, parserState)> = tokens => {
  (parseLet->skip(parseEOF))(tokens, {index: 0})->Result.map(((ast, _)) => ast)
}
