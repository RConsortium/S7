# new_object: reports all property type errors

    Code
      range(start = "x", end = "y")
    Error <simpleError>
      R7<range> object properties are invalid:
      - R7<range>@start must be <integer> or <double>, not <character>
      - R7<range>@end must be <integer> or <double>, not <character>

# new_object: checks are arguments are properties

    Code
      foo <- new_class("foo")
      foo(1)
    Error <simpleError>
      unused argument (1)
    Code
      foo(1, 2)
    Error <simpleError>
      unused arguments (1, 2)
    Code
      foo(x = 1)
    Error <simpleError>
      unused argument (x = 1)
    Code
      foo(x = 1, y = 2)
    Error <simpleError>
      unused arguments (x = 1, y = 2)

# print()/str() gives useful display

    Code
      str(range(1, 10))
    Output
      R7<range>
      @ start :  num 1
      @ end   :  num 10
      @ length:  num 9
    Code
      str(list(text("b"), number(50)))
    Output
      List of 2
       $ : R7<text> chr "b"
       $ : R7<number> num 50

# print()/str() nests properties correctly

    Code
      str(klass(x = 10, y = range(1, 10)))
    Output
      R7<klass>
      @ x:  num 10
      @ y:  R7<range>
       .. @ start :  num 1
       .. @ end   :  num 10
       .. @ length:  num 9

