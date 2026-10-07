# generates meaningful constructors

    Code
      Root@constructor
    Output
      function () 
      {
          S7::new_object(S7::S7_object())
      }
      <environment: 0x0>
    Code
      Props@constructor
    Output
      function (x = integer(0), y = integer(0)) 
      {
          x
          y
          S7::new_object(S7::S7_object(), x = x, y = y)
      }
      <environment: 0x0>
    Code
      foo2@constructor
    Output
      function (.data = character(0)) 
      S7::new_object(foo(.data = .data))
      <environment: 0x0>
    Code
      foo3@constructor
    Output
      function (.data = character(0)) 
      S7::new_object(foo2(.data = .data))
      <environment: 0x0>

# can generate constructors for S3 classes

    Code
      new_constructor(class_factor, list())
    Output
      function (.data = integer(), levels = NULL) 
      new_object(new_factor(.data = .data, levels = levels))
      <environment: 0x0>
    Code
      new_constructor(class_factor, as_properties(list(x = class_numeric, y = class_numeric)))
    Output
      function (.data = integer(), levels = NULL, x = integer(0), y = integer(0)) 
      new_object(new_factor(.data = .data, levels = levels), x = x, 
          y = y)
      <environment: 0x0>

# can generate constructor for inherited abstract classes

    Code
      foo2@constructor
    Output
      function (x = numeric(0)) 
      {
          x
          S7::new_object(S7::S7_object(), x = x)
      }
      <environment: 0x0>
    Code
      foo3@constructor
    Output
      function (x = numeric(0), y = numeric(0)) 
      {
          x
          y
          S7::new_object(S7::S7_object(), x = x, y = y)
      }
      <environment: 0x0>

# can use `...` in parent constructor

    Code
      bar@constructor
    Output
      function (..., y = numeric(0)) 
      S7::new_object(foo(...), y = y)
      <environment: 0x0>

# forwarding constructor passes overrides to both parent and object

    Code
      new_constructor(P, as_properties(list(a = new_property(class_double, default = 99),
      b = class_double)))
    Output
      function (..., a = 99, b = numeric(0)) 
      new_object(P(a = a, ...), a = a, b = b)
      <environment: 0x0>

