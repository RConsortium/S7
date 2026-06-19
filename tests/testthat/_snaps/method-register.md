# method registration adds messages when overwriting

    Code
      method(foo, class_character) <- (function(x) "c")
      method(foo, class_character) <- (function(x) "C")
    Message
      Overwriting method foo(<character>)

# method registration checks argument types

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

# method registration resolves external classes outside packages

    Code
      env$g(S7_object())
    Condition
      Error:
      ! Can't find method for `g(<S7_object>)`.

# method registration validates deferred external-class methods

    Code
      local_package("pkg_invalid_deferred_external_class_method", foo := new_generic(
        "x"), ext := new_external_class("notloaded.pkg"), method(foo, ext) <-
        (function(y) "x"))
    Condition
      Error in `method<-`:
      ! foo() dispatches on `x`, but foo(<notloaded.pkg::ext>) has arguments `y`.

# method unregistration resolves loaded external-class methods in packages

    Code
      downstream$foo(S7_object())
    Condition
      Error:
      ! Can't find method for `foo(<S7_object>)`.

---

    Code
      downstream$foo(S7_object())
    Condition
      Error:
      ! Can't find method for `foo(<S7_object>)`.

# method unregistration removes deferred unions regardless of order

    Code
      downstream$foo(upstream$Ext())
    Condition
      Error:
      ! Can't find method for `foo(<upstream_external_union_unregister::Ext>)`.

---

    Code
      downstream$foo(upstream$Ext())
    Condition
      Error:
      ! Can't find method for `foo(<upstream_external_union_unregister::Ext>)`.

# method unregistration removes an S7 method via NULL assignment

    Code
      foo("x")
    Condition
      Error:
      ! Can't find method for `foo(<character>)`.

# method unregistration removes a method with a multi-dispatch signature

    Code
      foo(A(), B())
    Condition
      Error:
      ! Can't find method for generic `foo(x, y)`:
      - x: <S7::A>
      - y: <S7::B>

# as_signature() accepts a length-1 list for single dispatch (#555)

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

# as_signature() requires a list of the correct length for multiple dispatch

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
      foo := new_generic("x")
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
      foo := new_generic("x", function(x) S7_dispatch())
      check_method(function(x, y) { }, foo)
    Condition
      Error:
      ! foo() generic lacks `...` so method formals must match generic formals exactly.
      - generic formals: foo(x)
      - method formals:  foo(x, y)

# check_method rejects primitive functions

    Code
      foo := new_generic("x")
      check_method(log, foo)
    Condition
      Error:
      ! foo(???) must be a function.

# check_method warn if default arguments don't match

    Code
      foo := new_generic("x", function(x, ..., z = 2, y = 1) S7_dispatch())
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

