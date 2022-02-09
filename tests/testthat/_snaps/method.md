# method registration: S3 registration requires single R7 class

    Code
      method(sum, list(foo, foo)) <- (function(x, ...) "foo")
    Error <simpleError>
      When registering methods for S3 generic sum(), signature be a single R7 class
    Code
      method(sum, s3_class("foo")) <- (function(x, ...) "foo")
    Error <simpleError>
      When registering methods for S3 generic sum(), signature be a single R7 class

# method registration: checks argument types

    Code
      x <- 10
      method(x, "character") <- (function(x) ...)
    Error <simpleError>
      `generic` must be a function, not a <double>
    Code
      method(foo, 1) <- (function(x) ...)
    Error <simpleError>
      Can't convert `signature[[1]]` to a valid class. Class specification must be an R7 class object, the result of `s3_class()`, an S4 class object, or a base constructor function, not a <double>.

# check_method errors if the functions are not compatible

    Code
      foo <- new_generic("foo", dispatch_args = "x")
      check_method(1, "character", foo)
    Error <simpleError>
      foo(<character>) must be a function
    Code
      check_method(function(y) { }, "character", foo)
    Error <simpleError>
      foo() dispatches on `x`, but foo(<character>) has arguments `y`
    Code
      check_method(function(x = "foo") { }, "character", foo)
    Error <simpleError>
      In foo(<character>), dispatch arguments (`x`) must not have default values
    Code
      check_method(function(x, y, ...) { }, "character", foo)
    Error <simpleError>
      In foo(<character>), `...` must come immediately after dispatch args (`x`)

# check_method warn if default arguments don't match

    Code
      foo <- new_generic("foo", function(x, ..., z = 2, y = 1) method_call())
      check_method(function(x, ..., y = 1) { }, "character", foo)
    Warning <simpleWarning>
      foo(<character>) doesn't have argument `z`
    Code
      check_method(function(x, ..., y = 1, z = 1) { }, "character", foo)
    Warning <simpleWarning>
      In foo(<character>), default value of `z` is not the same as the generic
      - Generic: 2
      - Method:  1

# R7_method printing

    Code
      method(foo, list(text, "integer"))
    Output
      <R7_method> method(foo, list(text, "integer"))
      function (x, y, ...) 
      paste0("bar:", x, y)
      <environment: 0x0>

