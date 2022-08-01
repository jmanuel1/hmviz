// Based on an idea inspired by https://arxiv.org/pdf/1202.2922v1.pdf from https://josh-hs-ko.github.io/blog/0023/

type rec traceStep<'s, 'a> = Intermediate('s, traceStep<'s, 'a>) | Return('a, 's)

type tracedState<'s, 'a> = 's => traceStep<'s, 'a>

let return : 'a => tracedState<'s, 'a> = (val, state) => {
  Return(val, state)
}

let mark : tracedState<'s, ()> = state => {
  Intermediate(state, return((), state))
}

let rec joinTrace : traceStep<'s, tracedState<'s, 'a>> => traceStep<'s, 'a> = trace => {
  switch trace {
    | Intermediate(state, trace') => Intermediate(state, joinTrace(trace'))
    | Return(ma, state) => ma(state)
  }
}

let join : tracedState<'s, tracedState<'s, 'a>> => tracedState<'s, 'a> = (mma, state) => {
  let ma = mma(state)
  joinTrace(ma)
}

let rec mapTrace : (traceStep<'s, 'a>, 'a => 'b) => traceStep<'s, 'b> = (trace, f) => {
  switch trace {
    | Intermediate(state, trace') => Intermediate(state, trace'->mapTrace(f))
    | Return(val, state) => Return(f(val), state)
  }
}

let map : (tracedState<'s, 'a>, 'a => 'b) => tracedState<'s, 'b> = (ma, f, state) => {
  ma(state)->mapTrace(f)
}

let flatMap : (tracedState<'s, 'a>, 'a => tracedState<'s, 'b>) => tracedState<'s, 'b> = (ma, f) => join(ma->map(f))

let get : tracedState<'s, 's> = state => return(state, state)

let modify : ('s => 's) => tracedState<'s, ()> = (f, state) => return((), f(state))

let run : (tracedState<'s, 'a>, 's) => traceStep<'s, 'a> = (ma, s) => {
  ma(s)
}
