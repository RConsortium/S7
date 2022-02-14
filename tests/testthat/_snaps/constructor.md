# generates meaningful constructors

    Code
      new_constructor(R7_object, list())
    Output
      function () 
      new_object(NULL)
      <environment: namespace:R7>
    Code
      new_constructor(R7_object, as_properties(list(x = "numeric", y = "numeric")))
    Output
      function (x, y) 
      new_object(NULL, x = x, y = y)
      <environment: namespace:R7>
    Code
      foo <- new_class("foo", parent = "character")
      new_constructor(foo, list())
    Output
      function (.data) 
      new_object(foo(.data = .data))
      <environment: 0x0>
    Code
      foo2 <- new_class("foo2", parent = foo)
      new_constructor(foo2, list())
    Output
      function (.data) 
      new_object(foo2(.data = .data))
      <environment: 0x0>

# can generate constructors for S3 classes

    Code
      new_constructor(s3_factor, list())
    Output
      function (.data, levels) 
      new_object(new_factor(.data = .data, levels = levels))
      <environment: 0x0>
    Code
      new_constructor(s3_factor, as_properties(list(x = "numeric", y = "numeric")))
    Output
      function (.data, levels, x, y) 
      new_object(new_factor(.data = .data, levels = levels), x = x, 
          y = y)
      <environment: 0x0>

