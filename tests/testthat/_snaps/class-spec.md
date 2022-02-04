# can get class from base constructor

    as_class(): could not find base class corresponding to supplied constructor function

# as_class gives informative errors

    Code
      as_class("foo")
    Error <simpleError>
      as_class(): Can't find base class called 'foo'
    Code
      as_class(TRUE)
    Error <simpleError>
      as_class(): class specification must be an R7 class object, the result of `s3_class()`, an S4 class object, or a base constructor function, not a <logical>.

# s3_class() checks its inputs

    Code
      s3_class(1)
    Error <simpleError>
      `class` must be a character vector

