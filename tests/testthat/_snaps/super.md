# super(): checks to

    Code
      foo <- new_class("foo")
      super(foo(), class_character)
    Error <simpleError>
      <foo> doesn't inherit from <character>

# super(): displays nicely

    Code
      f1 <- super(foo2(), foo1)
      f1
    Output
      super(<foo2>, <foo1>)
    Code
      str(list(f1))
    Output
      List of 1
       $ : super(<foo2>, <foo1>)

