# S7_methods() validates inputs

    Code
      S7_methods()
    Condition
      Error in `S7_methods()`:
      ! Must supply exactly one of `generic` or `class`.
    Code
      S7_methods(generic = new_generic("g", "x"), class = class_integer)
    Condition
      Error in `S7_methods()`:
      ! Must supply exactly one of `generic` or `class`.
    Code
      S7_methods(generic = "not a generic")
    Condition
      Error in `S7_methods()`:
      ! `generic` must be an S7 generic.

