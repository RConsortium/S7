# method errors on invalid inputs

    `signature` must be a list of <R7_class> or a <character>:
    - `signature[1]`: is <numeric>

---

    `signature` must be a list of <R7_class> or a <character>:
    - `signature[1]`: is <numeric>

---

    `signature` must be a list of <R7_class> or a <character>:
    - `signature[1]`: is <logical>
    - `signature[2]`: is <logical>

# method errors if no method is defined for that class

    Can't find method for generic `foo()` with classes:
    - x: <blah>

# method_compatible errors if the functions are not compatible

    Code
      foo <- new_generic("foo", dispatch_args = "x")
      method_compatible(function(y) { }, foo)
    Error <simpleError>
      `method` doesn't match generic dispatch arg
    Code
      method_compatible(function(x = "foo") { }, foo)
    Error <simpleError>
      Dispatch arguments must not have default values
    Code
      method_compatible(function(x, y, ...) { }, foo)
    Error <simpleError>
      ... must immediately follow dispatch args

# method_compatible warn if default arguments don't match

    Code
      foo <- new_generic("foo", function(x, ..., z = 2, y = 1) method_call())
      method_compatible(function(x, ..., y = 1) { }, foo)
    Warning <simpleWarning>
      Argument `z` is missing from method
    Output
      [1] TRUE
    Code
      method_compatible(function(x, ..., y = 1, z = 1) { }, foo)
    Warning <simpleWarning>
      Default value is not the same as the generic
      - Generic: z = 2
      - Method:  z = 1
    Output
      [1] TRUE

# R7_method printing

    Code
      method(foo, list(text, "integer"))
    Output
      <R7_method> method(foo, list(text, "integer"))
      function (x, y, ...) 
      paste0("bar:", x, y)
      <environment: 0x0>

