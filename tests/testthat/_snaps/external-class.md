# displays nicely

    Code
      print(foo)
    Output
      <S7_external_class> package::name

# constructor_fun must yield a class

    Code
      new_external_class("S7", "foo", function() 1)
    Error <simpleError>
      `constructor_fun()` must yield an S7 class

