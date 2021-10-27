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

