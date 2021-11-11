# R7_class: can be printed

    Code
      my_class
    Output
      <R7_class>
      @name my_class
      @parent <R7_object>
      @properties

# classes can use unions in properties

    <my_class>@name must be of class <character>, <factor>:
    - `value` is of class <numeric>

# generates meaningful constructors

    Code
      foo <- new_class("foo")
      foo@constructor
    Output
      function () 
      new_object(.data = NULL)
      <environment: namespace:R7>
    Code
      foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"))
      foo@constructor
    Output
      function (x, y) 
      new_object(.data = NULL, x = x, y = y)
      <environment: namespace:R7>
    Code
      foo <- new_class("foo", parent = "character")
      foo@constructor
    Output
      function (.data) 
      new_object(character(.data = .data))
      <environment: 0x0>
    Code
      foo2 <- new_class("foo2", parent = foo)
      foo2@constructor
    Output
      function (.data) 
      new_object(foo(.data = .data))
      <environment: 0x0>
    Code
      foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"),
      constructor = function() new_object(x = 1, y = 2))
      foo2 <- new_class("foo2", parent = foo, properties = list(z = "numeric"))
      foo2@constructor
    Output
      function (z) 
      new_object(foo(), z = z)
      <environment: 0x0>

