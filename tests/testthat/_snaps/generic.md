# new_generic needs fun or signature

    Must call `new_generic()` with at least one of `signature` or `fun`

# check_signature produces informative errors

    Code
      check_signature(1)
    Error <simpleError>
      `signature` must be a character vector
    Code
      check_signature(character())
    Error <simpleError>
      `signature` must have at least one component

# R7_generic printing

    Code
      foo
    Output
      <R7_generic> function (x, y, z, ...)  with 3 methods:
      1: method(foo, list("character", "integer", "character"))
      2: method(foo, list("character", "integer", "logical"))
      3: method(foo, list("character", text, "character"))

# R7_generic printing with long / many arguments

    Code
      foo
    Output
      <R7_generic> function (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, 
          r, s, t, u, v, w, x, y, z, ...)  with 0 methods:

# check_generic produces informative errors

    Code
      check_generic("x")
    Error <simpleError>
      `fun` must be a function
    Code
      check_generic(function() { })
    Error <simpleError>
      `fun` must contain a call to `method_call()`
