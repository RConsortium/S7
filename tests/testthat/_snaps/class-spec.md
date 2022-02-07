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
      Can't convert `TRUE` to a valid class. Class specification must be an R7 class object, the result of `s3_class()`, an S4 class object, or a base constructor function, not a <logical>.

# s3_class() checks its inputs

    Code
      s3_class(1)
    Error <simpleError>
      `class` must be a character vector

---

    Code
      s3_class("foo", function(x) { })
    Error <simpleError>
      First argument to `constructor` must be .data
    Code
      s3_class("foo", function(.data, ...) { })
    Error <simpleError>
      `constructor` can not use ...

# default s3_class constructor errors

    Code
      class_construct(s3_class("foo"))
    Error <simpleError>
      S3 class <foo> doesn't have a constructor

