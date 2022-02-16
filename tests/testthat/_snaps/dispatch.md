# generics pass ... to methods

    unused argument (z = 2)

# method lookup fails with informative messages

    Can't find method for generic `foo()` with classes:
    - x: <logical>
    - y: <MISSING>

---

    Can't find method for generic `foo()` with classes:
    - x: <logical>
    - y: <list>

---

    Can't find method for generic `foo()` with classes:
    - x: <tbl_df>, <tbl>, <data.frame>
    - y: <POSIXct>, <POSIXt>

# method introspection: errors on invalid inputs

    Code
      method(print, 1)
    Error <simpleError>
      `generic` must be an <R7_generic>
    Code
      foo <- new_generic("foo", "x")
      method(foo)
    Error <simpleError>
      Must supply exactly one of `classes` and `objects`
    Code
      method(foo, 1)
    Error <simpleError>
      Can't convert `signature` to a valid class. Class specification must be an R7 class object, the result of `S3_class()`, an S4 class object, or a base constructor function, not a <double>.
    Code
      method(foo, new_union("integer", "double"))
    Error <simpleError>
      Can't dispatch on unions; must be a concrete type
    Code
      foo2 <- new_generic("foo2", c("x", "y"))
      method(foo2, objects = list("character"))
    Error <simpleError>
      `objects` must be length 2

# errors if no method found

    Code
      method(foo, list())
    Error <simpleError>
      Can't convert `signature` to a valid class. Class specification must be an R7 class object, the result of `S3_class()`, an S4 class object, or a base constructor function, not a <list>.
    Code
      method(foo, list("blah"))
    Error <simpleError>
      Can't convert `signature` to a valid class. Class specification must be an R7 class object, the result of `S3_class()`, an S4 class object, or a base constructor function, not a <list>.

