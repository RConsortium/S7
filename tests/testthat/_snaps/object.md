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

# printing R7 objects work

    Code
      print(x)
    Output
      <range> <R7_object>
      @start   1
      @end    10
      @length  9

# printing R7 classes work

    Code
      range
    Output
      <R7_class>
      @name range
      @parent <R7_object>
      @properties
       $start  <numeric>
       $end    <numeric>
       $length <numeric>

# str with simple R7 objects work

    Code
      str(range(1, 2))
    Output
       <range/R7_object> List of 2
       .. $ start: num 1
       .. $ end  : num 2

# str with R7 objects of base classes work

    Code
      str(list(text("b"), number(50)))
    Output
      List of 2
       $ : <text/character/R7_object>  chr "b"
       $ : <number/numeric/R7_object>  num 50

# str R7 classes work

    Code
      str(range)
    Output
      <range/R7_object> constructor function (start, end)  
       .. - attr(*, "name")= chr "range"
       .. - attr(*, "parent")= <R7_object> constructor
       .. - attr(*, "properties")=List of 3
       ..  ..$ start :List of 4
       ..  .. ..$ name  : chr "start"
       ..  .. ..$ class : chr "numeric"
       ..  .. ..$ getter: NULL
       ..  .. ..$ setter: NULL
       ..  .. ..- attr(*, "class")= chr "R7_property"
       ..  ..$ end   :List of 4
       ..  .. ..$ name  : chr "end"
       ..  .. ..$ class : chr "numeric"
       ..  .. ..$ getter: NULL
       ..  .. ..$ setter: NULL
       ..  .. ..- attr(*, "class")= chr "R7_property"
       ..  ..$ length:List of 4
       ..  .. ..$ name  : chr "length"
       ..  .. ..$ class : chr "numeric"
       ..  .. ..$ getter:function (x)  
       ..  .. ..$ setter:function (x, value)  
       ..  .. ..- attr(*, "class")= chr "R7_property"
       .. - attr(*, "constructor")=function (start, end)  
       .. - attr(*, "validator")=function (x)  

