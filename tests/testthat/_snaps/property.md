# prop: does not use partial matching

    Can't find property <range>@st

# prop<-: errors if the property doesn't exist

    Code
      x@foo <- 10
    Error <simpleError>
      Can't find property <range>@foo

# @: does not use partial matching

    Can't find property <range>@st

# @: falls back to `base::@` for non-R7 objects

    Code
      "foo"@blah
    Error <simpleError>
      trying to get slot "blah" from an object of a basic class ("character") with no slots
    Code
      NULL@blah
    Error <simpleError>
      trying to get slot "blah" from an object of a basic class ("NULL") with no slots

# new_property validates name

    Code
      new_property(1)
    Error <simpleError>
      `name` must be a single string
    Code
      new_property("")
    Error <simpleError>
      `name` must not be "" or NA

# properties can be base, S3, S4, R7, or R7 union

    Code
      my_class
    Output
      <R7_class>
      @ name  :  my_class
      @ parent: <R7_object>
      @ properties:
       $ anything: <ANY>                 
       $ base    : <integer>             
       $ s3      : <factor>              
       $ s4      : <class_s4>            
       $ r7      : <class_r7>            
       $ r7_union: <integer> or <logical>

---

    Code
      my_obj@base <- "x"
    Error <simpleError>
      <my_class>@base must be of class <integer>, not <character>
    Code
      my_obj@s3 <- "x"
    Error <simpleError>
      <my_class>@s3 must be of class <factor>, not <character>
    Code
      my_obj@s4 <- "x"
    Error <simpleError>
      <my_class>@s4 must be of class <class_s4>, not <character>
    Code
      my_obj@r7 <- "x"
    Error <simpleError>
      <my_class>@r7 must be of class <class_r7>, not <character>
    Code
      my_obj@r7_union <- "x"
    Error <simpleError>
      <my_class>@r7_union must be of class <integer> or <logical>, not <character>

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
      Can't convert `property$x` to a valid class. Class specification must be an R7 class object, the result of `s3_class()`, an S4 class object, or a base constructor function, not a <double>.
    Code
      as_properties(list(x = "character", x = "character"))
    Error <simpleError>
      `properties` names must be unique

