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
      foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"))
      foo@constructor
    Output
      function (x, y) 
      new_object(NULL, x = x, y = y)
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

