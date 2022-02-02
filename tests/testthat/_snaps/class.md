# R7_class: can be printed

    Code
      my_class
    Output
      <R7_class>
      @name my_class
      @parent <R7_object>
      @properties

# R7_class: str yields all details when used at top-level

    Code
      str(my_class)
    Output
      <my_class/R7_object> constructor
      @ name       :  chr "my_class"
      @ parent     :  <R7_object> constructor
      @ properties :  list()
      @ constructor: function ()  
      @ validator  : function (x)  
      @ class      :  chr [1:2] "R7_class" "R7_object"
    Code
      str(list(my_class))
    Output
      List of 1
       $ : <my_class/R7_object> constructor

# classes can't inherit from S4 or class unions

    Code
      new_class("test", parent = parentS4)
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type, not an S4 class.
    Code
      new_class("test", parent = new_union("character"))
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type, not a class union.

# constructor  types check their values

    `.data` must be <integer> not <character>

