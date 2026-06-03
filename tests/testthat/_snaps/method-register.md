# method registration / adds messages when overwriting

    Code
      method(foo, class_character) <- (function(x) "c")
      method(foo, class_character) <- (function(x) "c")
    Message
      Overwriting method foo(<character>)

# method registration / rejects class_missing on S3 generics

    Code
      method(s3_gen, class_missing) <- (function(x) "missing")
    Condition
      Error:
      ! `class_missing` not supported for non-operator S3 generics.

# method registration / can register S7 method for S4 generic

    Class has not been registered with S4; please call S4_register(S4foo).

# method registration / checks argument types

    Code
      x <- 10
      method(x, class_character) <- (function(x) ...)
    Condition
      Error in `method<-`:
      ! `generic` must be a function, not a <double>.
    Code
      method(foo, 1) <- (function(x) ...)
    Condition
      Error in `as_class()`:
      ! Can't convert `signature` to a valid class.
      Class specification must be one of the following, not a <double>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class

# method unregistration / removes S7 method via NULL assignment

    Code
      foo("x")
    Condition
      Error:
      ! Can't find method for `foo(<character>)`.

# method unregistration / removes method with multi-dispatch signature

    Code
      foo(A(), B())
    Condition
      Error:
      ! Can't find method for generic `foo(x, y)`:
      - x: <S7::A>
      - y: <S7::B>

# method unregistration / errors when unregistering from an S3 generic

    Code
      method(sum, foo) <- NULL
    Condition
      Error in `method<-`:
      ! Can't unregister methods for S3 generics

---

    Code
      method(base_sum, foo) <- NULL
    Condition
      Error in `method<-`:
      ! Can't unregister methods for S3 generics

# method unregistration / errors when unregistering from an S4 generic

    Code
      method(removeS4, S4foo) <- NULL
    Condition
      Error in `method<-`:
      ! Can't unregister methods for S4 generics

# as_signature() / accepts a length-1 list for single dispatch (#555)

    Code
      as_signature(list(1), foo)
    Condition
      Error in `as_class()`:
      ! Can't convert `signature` to a valid class.
      Class specification must be one of the following, not a <double>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class

# as_signature() / requires a list of the correct length for multiple dispatch

    Code
      as_signature(class_character, foo)
    Condition
      Error:
      ! `signature` must be a list for multidispatch generics.
    Code
      as_signature(list(class_character), foo)
    Condition
      Error:
      ! `signature` must be length 2.

# S7_signature has format and print methods

    Code
      print(sig)
    Output
      <integer>, <character>

# check_method complains if the functions are not compatible

    Code
      foo <- new_generic("foo", "x")
      check_method(1, foo)
    Condition
      Error:
      ! foo(???) must be a function.
    Code
      check_method(function(y) { }, foo)
    Condition
      Error:
      ! foo() dispatches on `x`, but foo(???) has arguments `y`.
    Code
      check_method(function(x = "foo") { }, foo)
    Condition
      Error:
      ! In foo(???), dispatch arguments (`x`) must not have default values.
    Code
      check_method(function(x, y, ...) { }, foo)

---

    Code
      foo <- new_generic("foo", "x", function(x) S7_dispatch())
      check_method(function(x, y) { }, foo)
    Condition
      Error:
      ! foo() generic lacks `...` so method formals must match generic formals exactly.
      - generic formals: foo(x)
      - method formals:  foo(x, y)

# check_method rejects primitive functions

    Code
      foo <- new_generic("foo", "x")
      check_method(log, foo)
    Condition
      Error:
      ! foo(???) must be a function.

# check_method warn if default arguments don't match

    Code
      foo <- new_generic("foo", "x", function(x, ..., z = 2, y = 1) S7_dispatch())
      check_method(function(x, ..., y = 1) { }, foo)
    Condition
      Warning:
      foo(???) doesn't have argument `z`
    Code
      check_method(function(x, ..., y = 1, z = 1) { }, foo)
    Condition
      Warning:
      In foo(???), default value of `z` is not the same as the generic
      - Generic: 2
      - Method:  1

# S7_method printing

    Code
      method(foo, list(class_integer, class_integer))
    Output
      <S7_method> method(foo, list(class_integer, class_integer))
      function (x, y, ...) 
      {
          paste0("bar:", x, y)
      }
      <environment: 0x0>

