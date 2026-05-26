# S7_class_desc() formats every supported class spec

    Code
      S7_class_desc(1L)
    Condition
      Error:
      ! Can't convert `class` to a valid class.
      Class specification must be one of the following, not a <integer>:
       * An S7 class object
       * The result of `new_S3_class()`
       * An S4 class object
       * A base class

# as_class gives informative errors

    Code
      as_class("foo")
    Condition
      Error:
      ! Can't convert `"foo"` to a valid class.
      Class specification must be one of the following, not a <character>:
       * An S7 class object
       * The result of `new_S3_class()`
       * An S4 class object
       * A base class
    Code
      as_class(TRUE)
    Condition
      Error:
      ! Can't convert `TRUE` to a valid class.
      Class specification must be one of the following, not a <logical>:
       * An S7 class object
       * The result of `new_S3_class()`
       * An S4 class object
       * A base class

