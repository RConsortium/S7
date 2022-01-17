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
      <range>
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
       $start  <double>
       $end    <double>
       $length <double>

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
       $ : <number/double/R7_object>  num 50

# str R7 classes work

    Code
      str(range)
    Output
       <R7_class/R7_object> function (start, end)  
       .. - attr(*, "name")= chr "range"
       .. - attr(*, "parent")= <R7_class/R7_object> function ()  
       ..  ..- attr(*, "name")= chr "R7_object"
       ..  ..- attr(*, "properties")= list()
       ..  ..- attr(*, "constructor")=function ()  
       ..  ..- attr(*, "validator")=function (x)  
       .. - attr(*, "properties")=List of 3
       ..  ..$ start :List of 4
       ..  .. ..$ name  : chr "start"
       ..  .. ..$ class : <R7_class/R7_object> function (.data)  
       ..  .. .. ..- attr(*, "name")= chr "double"
       ..  .. .. ..- attr(*, "parent")= <R7_class/R7_object> function ()  
       ..  .. .. .. ..- attr(*, "name")= chr "R7_object"
       ..  .. .. .. ..- attr(*, "properties")= list()
       ..  .. .. .. ..- attr(*, "constructor")=function ()  
       ..  .. .. .. ..- attr(*, "validator")=function (x)  
       ..  .. .. ..- attr(*, "properties")= list()
       ..  .. .. ..- attr(*, "constructor")=function (.data)  
       ..  .. .. ..- attr(*, "validator")=function (x)  
       ..  .. ..$ getter: NULL
       ..  .. ..$ setter: NULL
       ..  .. ..- attr(*, "class")= chr "R7_property"
       ..  ..$ end   :List of 4
       ..  .. ..$ name  : chr "end"
       ..  .. ..$ class : <R7_class/R7_object> function (.data)  
       ..  .. .. ..- attr(*, "name")= chr "double"
       ..  .. .. ..- attr(*, "parent")= <R7_class/R7_object> function ()  
       ..  .. .. .. ..- attr(*, "name")= chr "R7_object"
       ..  .. .. .. ..- attr(*, "properties")= list()
       ..  .. .. .. ..- attr(*, "constructor")=function ()  
       ..  .. .. .. ..- attr(*, "validator")=function (x)  
       ..  .. .. ..- attr(*, "properties")= list()
       ..  .. .. ..- attr(*, "constructor")=function (.data)  
       ..  .. .. ..- attr(*, "validator")=function (x)  
       ..  .. ..$ getter: NULL
       ..  .. ..$ setter: NULL
       ..  .. ..- attr(*, "class")= chr "R7_property"
       ..  ..$ length:List of 4
       ..  .. ..$ name  : chr "length"
       ..  .. ..$ class : <R7_class/R7_object> function (.data)  
       ..  .. .. ..- attr(*, "name")= chr "double"
       ..  .. .. ..- attr(*, "parent")= <R7_class/R7_object> function ()  
       ..  .. .. .. ..- attr(*, "name")= chr "R7_object"
       ..  .. .. .. ..- attr(*, "properties")= list()
       ..  .. .. .. ..- attr(*, "constructor")=function ()  
       ..  .. .. .. ..- attr(*, "validator")=function (x)  
       ..  .. .. ..- attr(*, "properties")= list()
       ..  .. .. ..- attr(*, "constructor")=function (.data)  
       ..  .. .. ..- attr(*, "validator")=function (x)  
       ..  .. ..$ getter:function (x)  
       ..  .. ..$ setter:function (x, value)  
       ..  .. ..- attr(*, "class")= chr "R7_property"
       .. - attr(*, "constructor")=function (start, end)  
       .. - attr(*, "validator")=function (x)  

