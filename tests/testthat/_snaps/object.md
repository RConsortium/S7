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
       $start  <numeric>
       $end    <numeric>
       $length <numeric>

# str with simple R7 objects work

    Code
      str(range(1, 2))
    Output
      <range/R7_object>
      @ start :  num 1
      @ end   :  num 2
      @ length:  num 1

# str with R7 objects of base classes work

    Code
      str(list(text("b"), number(50)))
    Output
      List of 2
       $ : <text/character/R7_object> chr "b"
       $ : <number/numeric/R7_object> num 50

# str R7 classes work

    Code
      str(range)
    Output
      <range/R7_object> constructor
      @ name       :  chr "range"
      @ parent     :  <R7_object> constructor
      @ properties : List of 3
       .. $ start : <R7_property> 
       .. .. $ name  :  chr "start"
       .. .. $ class :  <numeric/R7_object> constructor
       .. .. $ getter:  NULL
       .. .. $ setter:  NULL
       .. $ end   : <R7_property> 
       .. .. $ name  :  chr "end"
       .. .. $ class :  <numeric/R7_object> constructor
       .. .. $ getter:  NULL
       .. .. $ setter:  NULL
       .. $ length: <R7_property> 
       .. .. $ name  :  chr "length"
       .. .. $ class :  <numeric/R7_object> constructor
       .. .. $ getter:  function (x)  
       .. .. $ setter:  function (x, value)  
      @ constructor:  function (start, end)  
      @ validator  :  function (x)  
      @ class      :  chr [1:2] "R7_class" "R7_object"

