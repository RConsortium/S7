# new_object: reports all property type errors

    Code
      range(start = "x", end = "y")
    Error <simpleError>
      <range> object properties are invalid:
      - <range>@start must be of class <integer> or <double>, not <character>
      - <range>@end must be of class <integer> or <double>, not <character>

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
      <range/R7_object>
      @ start :  num 1
      @ end   :  num 10
      @ length:  num 9
    Code
      str(list(text("b"), number(50)))
    Output
      List of 2
       $ : <text/character/R7_object> chr "b"
       $ : <number/double/R7_object> num 50

# print()/str() nests properties correctly

    Code
      str(klass(x = 10, y = range(1, 10)))
    Output
      <klass/R7_object>
      @ x:  num 10
      @ y:  <range/R7_object>
       .. @ start :  num 1
       .. @ end   :  num 10
       .. @ length:  num 9

