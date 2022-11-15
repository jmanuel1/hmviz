let mapError : result<'a, 'b> => ('b => 'c) => result<'a, 'c> = (result, f) => {
  switch result {
  | Error(b) => Error(f(b))
  | Ok(a) => Ok(a)
  }
}
