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

# method(): errors on invalid inputs

    Code
      method(print, 1)
    Error <simpleError>
      `generic` must be an <R7_generic>
    Code
      foo <- (function(x) { })
      method(foo, 1)
    Error <simpleError>
      `generic` must be an <R7_generic>
    Code
      method(foo, new_union("integer", "double"))
    Error <simpleError>
      `generic` must be an <R7_generic>

# errors if no method found

    Code
      method(foo, list())
    Error <simpleError>
      Can't find method for generic `foo()` with classes:
      - x: 
    Code
      method(foo, list("blah"))
    Error <simpleError>
      Can't convert `signature[[1]]` to a valid class. No base classes are called 'blah'

