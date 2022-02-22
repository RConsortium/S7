# R7 classes: print nicely

    Code
      foo2
    Output
      <R7_class>
      @ name  :  foo2
      @ parent: <foo1>
      @ properties:
       $ x: <integer>
       $ y: <integer>
    Code
      str(foo2)
    Output
      <foo2/foo1/R7_object> constructor
      @ name       :  chr "foo2"
      @ parent     :  <foo1/R7_object> constructor
      @ package    :  NULL
      @ properties : List of 2
       .. $ x: <R7_property> 
       ..  ..$ name   :  chr "x"
       ..  ..$ class  :  <R7_base_class>: <integer>
       ..  ..$ getter :  NULL
       ..  ..$ setter :  NULL
       ..  ..$ default:  NULL
       .. $ y: <R7_property> 
       ..  ..$ name   :  chr "y"
       ..  ..$ class  :  <R7_base_class>: <integer>
       ..  ..$ getter :  NULL
       ..  ..$ setter :  NULL
       ..  ..$ default:  NULL
      @ constructor:  function (x = missing_class, y = missing_class)  
      @ validator  :  NULL
    Code
      str(list(foo2))
    Output
      List of 1
       $ : <foo2/foo1/R7_object> constructor

# R7 classes: checks inputs

    Code
      new_class(1)
    Error <simpleError>
      `name` must be a single string
    Code
      new_class("foo", 1)
    Error <simpleError>
      Can't convert `parent` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a <double>.
    Code
      new_class("foo", package = 1)
    Error <simpleError>
      `package` must be a single string
    Code
      new_class("foo", constructor = 1)
    Error <simpleError>
      `constructor` must be a function
    Code
      new_class("foo", constructor = function() { })
    Error <simpleError>
      `constructor` must contain a call to `new_object()`
    Code
      new_class("foo", validator = function() { })
    Error <simpleError>
      `validator` must be function(self), not function()

# R7 classes: can't inherit from S4 or class unions

    Code
      new_class("test", parent = parentS4)
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type, not an S4 class.
    Code
      new_class("test", parent = new_union("character"))
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type, not an R7 union.

# new_object(): gives useful error if called directly

    Code
      new_object()
    Error <simpleError>
      `new_object()` must be called from within a constructor

# new_object(): validates object

    Code
      foo("x")
    Error <simpleError>
      <foo> object properties are invalid:
      - @x must be <double>, not <character>
    Code
      foo(-1)
    Error <simpleError>
      <foo> object is invalid:
      - x must be positive

# R7 object: displays nicely

    Code
      foo <- new_class("foo", properties = list(x = double, y = double))
      foo()
    Output
      <foo>
      @ x:  num(0) 
      @ y:  num(0) 
    Code
      str(list(foo()))
    Output
      List of 1
       $ : <foo>
        ..@ x:  num(0) 
        ..@ y:  num(0) 

# R7 object: displays objects with data nicely

    Code
      text <- new_class("text", character)
      text("x")
    Output
      <text> chr "x"
    Code
      str(list(text("x")))
    Output
      List of 1
       $ : <text> chr "x"

