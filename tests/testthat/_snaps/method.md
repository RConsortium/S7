# method registration: S3 registration requires a R7 class

    Code
      method(sum, new_S3_class("foo")) <- (function(x, ...) "foo")
    Error <simpleError>
      When registering methods for S3 generic sum(), signature must be an R7 class

# method registration: checks argument types

    Code
      x <- 10
      method(x, "character") <- (function(x) ...)
    Error <simpleError>
      `generic` must be a function, not a <double>
    Code
      method(foo, 1) <- (function(x) ...)
    Error <simpleError>
      Can't convert `signature` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a <double>.

# as_signature(): forbids list for single dispatch

    Code
      as_signature(list(1), foo)
    Error <simpleError>
      Can't convert `signature` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a <list>.

# as_signature(): requires a list of the correct length for multiple dispatch

    Code
      as_signature("character", foo)
    Error <simpleError>
      `signature` must be a list for multidispatch generics
    Code
      as_signature(list("character"), foo)
    Error <simpleError>
      `signature` must be length 2

# check_method errors if the functions are not compatible

    Code
      foo <- new_generic("foo", "x")
      check_method(1, foo)
    Error <simpleError>
      foo(???) must be a function
    Code
      check_method(function(y) { }, foo)
    Error <simpleError>
      foo() dispatches on `x`, but foo(???) has arguments `y`
    Code
      check_method(function(x = "foo") { }, foo)
    Error <simpleError>
      In foo(???), dispatch arguments (`x`) must not have default values
    Code
      check_method(function(x, y, ...) { }, foo)
    Error <simpleError>
      In foo(???), `...` must come immediately after dispatch args (`x`)

# check_method warn if default arguments don't match

    Code
      foo <- new_generic("foo", fun = function(x, ..., z = 2, y = 1) method_call())
      check_method(function(x, ..., y = 1) { }, foo)
    Warning <simpleWarning>
      foo(???) doesn't have argument `z`
    Code
      check_method(function(x, ..., y = 1, z = 1) { }, foo)
    Warning <simpleWarning>
      In foo(???), default value of `z` is not the same as the generic
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

