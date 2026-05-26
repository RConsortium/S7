# S7_class_desc() formats every supported class spec

    Code
      S7_class_desc(1L)
    Condition
      Error:
      ! Can't convert `class` to a valid class. Class specification must be an S7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <integer>.

# as_class gives informative errors

    Code
      as_class("foo")
    Condition
      Error:
      ! Can't convert `"foo"` to a valid class. Class specification must be an S7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <character>.
    Code
      as_class(TRUE)
    Condition
      Error:
      ! Can't convert `TRUE` to a valid class. Class specification must be an S7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <logical>.

