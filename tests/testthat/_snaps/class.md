# R7_class: can be printed

    Code
      my_class
    Output
      <R7_class>
      @ name  :  my_class
      @ parent: <R7_object>
      @ properties:

# R7_class: str yields all details when used at top-level

    Code
      str(my_class)
    Output
      <my_class/R7_object> constructor
      @ name       :  chr "my_class"
      @ parent     :  <R7_object> constructor
      @ properties :  list()
      @ constructor:  function ()  
      @ validator  :  function (x)  
    Code
      str(range)
    Output
      <range/R7_object> constructor
      @ name       :  chr "range"
      @ parent     :  <R7_object> constructor
      @ properties : List of 3
       .. $ start : <R7_property> 
       .. .. $ name  :  chr "start"
       .. .. $ class :  <R7_union>: <integer> or <double>
       .. .. $ getter:  NULL
       .. .. $ setter:  NULL
       .. $ end   : <R7_property> 
       .. .. $ name  :  chr "end"
       .. .. $ class :  <R7_union>: <integer> or <double>
       .. .. $ getter:  NULL
       .. .. $ setter:  NULL
       .. $ length: <R7_property> 
       .. .. $ name  :  chr "length"
       .. .. $ class :  <R7_union>: <integer> or <double>
       .. .. $ getter:  function (x)  
       .. .. $ setter:  function (x, value)  
      @ constructor:  function (start, end)  
      @ validator  :  function (x)  

# R7_class: str() summarises when nested

    Code
      list(range)
    Output
      [[1]]
      <R7_class>
      @ name  :  range
      @ parent: <R7_object>
      @ properties:
       $ start : <integer> or <double>
       $ end   : <integer> or <double>
       $ length: <integer> or <double>
      

# new_class() checks its inputs

    Code
      new_class(1)
    Error <simpleError>
      `name` must be a single string
    Code
      new_class("foo", 1)
    Error <simpleError>
      Can't convert `parent` to a valid class. Class specification must be an R7 class object, the result of `S3_class()`, an S4 class object, or a base constructor function, not a <double>.

# classes can't inherit from S4 or class unions

    Code
      new_class("test", parent = parentS4)
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type, not an S4 class.
    Code
      new_class("test", parent = new_union("character"))
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type, not a class union.

