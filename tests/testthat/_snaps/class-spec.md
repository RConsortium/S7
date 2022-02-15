# can get class from base constructor

    Can't convert `mean` to a valid class. Could not find base class corresponding to supplied constructor function

# as_class gives informative errors

    Code
      as_class("foo")
    Error <simpleError>
      Can't convert `"foo"` to a valid class. No base classes are called 'foo'
    Code
      as_class(TRUE)
    Error <simpleError>
      Can't convert `TRUE` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a <logical>.

