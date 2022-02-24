# property retrieval: retrieves the properties that exist & errors otherwise

    Can't find property <foo>@x

---

    Can't find property <foo>@x

# property retrieval: falls back to `base::@` for non-R7 objects

    Code
      "foo"@blah
    Error <simpleError>
      trying to get slot "blah" from an object of a basic class ("character") with no slots
    Code
      NULL@blah
    Error <simpleError>
      trying to get slot "blah" from an object of a basic class ("NULL") with no slots

# prop setting: can't set read-only properties

    Code
      obj@x <- 1
    Error <simpleError>
      Can't set read-only property <foo>@x

# prop setting: errors if the property doesn't exist or is wrong class

    Code
      x@foo <- 10
    Error <simpleError>
      object 'x' not found
    Code
      x@x <- "x"
    Error <simpleError>
      object 'x' not found

# new_property(): validates name

    Code
      new_property(1)
    Error <simpleError>
      `name` must be a single string
    Code
      new_property("")
    Error <simpleError>
      `name` must not be "" or NA

# new_property(): validates getter and settor

    Code
      new_property("x", getter = function(x) { })
    Error <simpleError>
      `getter` must be function(self), not function(x)
    Code
      new_property("x", setter = function(x, y, z) { })
    Error <simpleError>
      `setter` must be function(self, value), not function(x, y, z)

# new_property(): validates default

    Code
      new_property("foo", class = "integer", default = "x")
    Error <simpleError>
      `default` must be an instance of <integer>, not a <character>

# new_property(): displays nicely

    Code
      print(x)
    Output
      <R7_property> 
      $ name   :  chr "foo"
      $ class  :  <R7_base_class>: <integer>
      $ getter :  NULL
      $ setter :  NULL
      $ default:  NULL
    Code
      str(list(x))
    Output
      List of 1
       $ : <R7_property> 
        ..$ name   :  chr "foo"
        ..$ class  :  <R7_base_class>: <integer>
        ..$ getter :  NULL
        ..$ setter :  NULL
        ..$ default:  NULL

# properties can be base, S3, S4, R7, or R7 union

    Code
      my_class
    Output
      <R7_class>
      @ name  :  my_class
      @ parent: <R7_object>
      @ properties:
       $ anything: <ANY>                 
       $ null    : <NULL>                
       $ base    : <integer>             
       $ S3      : S3<factor>            
       $ S4      : S4<class_S4>          
       $ R7      : <class_R7>            
       $ R7_union: <integer> or <logical>

---

    Code
      my_obj@null <- "x"
    Error <simpleError>
      <my_class>@null must be <NULL>, not <character>
    Code
      my_obj@base <- "x"
    Error <simpleError>
      <my_class>@base must be <integer>, not <character>
    Code
      my_obj@S3 <- "x"
    Error <simpleError>
      <my_class>@S3 must be S3<factor>, not <character>
    Code
      my_obj@S4 <- "x"
    Error <simpleError>
      <my_class>@S4 must be S4<class_S4>, not <character>
    Code
      my_obj@R7 <- "x"
    Error <simpleError>
      <my_class>@R7 must be <class_R7>, not <character>
    Code
      my_obj@R7_union <- "x"
    Error <simpleError>
      <my_class>@R7_union must be <integer> or <logical>, not <character>

# as_properties() gives useful error messages

    Code
      as_properties(1)
    Error <simpleError>
      `properties` must be a list
    Code
      as_properties(list(1))
    Error <simpleError>
      `property[[1]]` is missing a name
    Code
      as_properties(list(x = 1))
    Error <simpleError>
      Can't convert `property$x` to a valid class. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a <double>.
    Code
      as_properties(list(x = "character", x = "character"))
    Error <simpleError>
      `properties` names must be unique

