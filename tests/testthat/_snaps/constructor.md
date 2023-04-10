# generates meaningful constructors

    Code
      new_constructor(S7_object, list())
    Output
      function () 
      new_object(NULL)
      <environment: namespace:S7>
    Code
      new_constructor(S7_object, as_properties(list(x = class_numeric, y = class_numeric)))
    Output
      function (x = class_missing, y = class_missing) 
      new_object(NULL, x = x, y = y)
      <environment: namespace:S7>
    Code
      foo <- new_class("foo", parent = class_character)
      new_constructor(foo, list())
    Output
      function (.data = class_missing) 
      new_object(foo(.data = .data))
      <environment: 0x0>
    Code
      foo2 <- new_class("foo2", parent = foo)
      new_constructor(foo2, list())
    Output
      function (.data = class_missing) 
      new_object(foo2(.data = .data))
      <environment: 0x0>

# can generate constructors for S3 classes

    Code
      new_constructor(class_factor, list())
    Output
      function (.data = integer(), levels = character()) 
      new_object(new_factor(.data = .data, levels = levels))
      <environment: 0x0>
    Code
      new_constructor(class_factor, as_properties(list(x = class_numeric, y = class_numeric)))
    Output
      function (.data = integer(), levels = character(), x = class_missing, 
          y = class_missing) 
      new_object(new_factor(.data = .data, levels = levels), x = x, 
          y = y)
      <environment: 0x0>

# can generate constructor for inherited abstract classes

    Code
      foo1 <- new_class("foo1", abstract = TRUE, properties = list(x = class_double))
      new_constructor(foo1, list())
    Output
      function () 
      new_object(S7_object())
      <environment: namespace:S7>
    Code
      new_constructor(foo1, as_properties(list(y = class_double)))
    Output
      function (y = class_missing) 
      new_object(S7_object(), y = y)
      <environment: namespace:S7>

# can use `...` in parent constructor

    Code
      new_constructor(foo, list(y = class_double))
    Output
      function (..., y = class_missing) 
      new_object(foo(... = ...), y = y)
      <environment: 0x0>

