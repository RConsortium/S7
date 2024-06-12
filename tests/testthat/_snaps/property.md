# property retrieval: retrieves the properties that exist & errors otherwise

    Can't find property <foo>@x

---

    Can't find property <foo>@x

# prop setting: can't set read-only properties

    Code
      obj@x <- 1
    Condition
      Error:
      ! Can't set read-only property <foo>@x

# prop setting: errors if the property doesn't exist or is wrong class

    Code
      obj <- foo(123)
      obj@foo <- 10
    Condition
      Error:
      ! Can't find property <foo>@foo
    Code
      obj@x <- "x"
    Condition
      Error:
      ! <foo>@x must be <double>, not <character>

# prop setting: validates all attributes if custom setter

    Code
      obj <- foo(y = 123, x = 123)
      obj@x <- "x"
    Condition
      Error:
      ! <foo>@y must be <double>, not <character>

# new_property(): validates getter and settor

    Code
      new_property(getter = function(x) { })
    Condition
      Error:
      ! `getter` must be function(self), not function(x)
    Code
      new_property(setter = function(x, y, z) { })
    Condition
      Error:
      ! `setter` must be function(self, value), not function(x, y, z)

# new_property(): validates default

    Code
      new_property(class_integer, default = "x")
    Condition
      Error in `new_property()`:
      ! `default` must be an instance of <integer>, not a <character>

# new_property(): displays nicely

    Code
      print(x)
    Output
      <S7_property> 
       $ name     : chr "foo"
       $ class    : <S7_base_class>: <integer>
       $ getter   : NULL
       $ setter   : NULL
       $ validator: NULL
       $ default  : NULL
    Code
      str(list(x))
    Output
      List of 1
       $ : <S7_property> 
        ..$ name     : chr "foo"
        ..$ class    : <S7_base_class>: <integer>
        ..$ getter   : NULL
        ..$ setter   : NULL
        ..$ validator: NULL
        ..$ default  : NULL

# properties can be base, S3, S4, S7, or S7 union

    Code
      my_class
    Output
      <my_class> class
      @ parent     : <S7_object>
      @ constructor: function(anything, null, base, S3, S4, S7, S7_union) {...}
      @ validator  : <NULL>
      @ properties :
       $ anything: <ANY>                 
       $ null    : <NULL>                
       $ base    : <integer>             
       $ S3      : S3<factor>            
       $ S4      : S4<class_S4>          
       $ S7      : <class_S7>            
       $ S7_union: <integer> or <logical>

---

    Code
      my_obj@null <- "x"
    Condition
      Error:
      ! <my_class>@null must be <NULL>, not <character>
    Code
      my_obj@base <- "x"
    Condition
      Error:
      ! <my_class>@base must be <integer>, not <character>
    Code
      my_obj@S3 <- "x"
    Condition
      Error:
      ! <my_class>@S3 must be S3<factor>, not <character>
    Code
      my_obj@S4 <- "x"
    Condition
      Error:
      ! <my_class>@S4 must be S4<class_S4>, not <character>
    Code
      my_obj@S7 <- "x"
    Condition
      Error:
      ! <my_class>@S7 must be <class_S7>, not <character>
    Code
      my_obj@S7_union <- "x"
    Condition
      Error:
      ! <my_class>@S7_union must be <integer> or <logical>, not <character>

# as_properties() gives useful error messages

    Code
      as_properties(1)
    Condition
      Error:
      ! `properties` must be a list
    Code
      as_properties(list(1))
    Condition
      Error:
      ! `properties[[1]]` must be named.
    Code
      as_properties(list(new_property(class_character)))
    Condition
      Error:
      ! `properties[[1]]` must have a name or be named.
    Code
      as_properties(list(x = 1))
    Condition
      Error:
      ! Can't convert `property$x` to a valid class. Class specification must be an S7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <double>.
    Code
      as_properties(list(x = class_character, x = class_character))
    Condition
      Error:
      ! `properties` names must be unique

# can validate with custom validator

    Code
      f <- foo(x = 1L)
      f@x <- 1:2
    Condition
      Error:
      ! <foo>@x must be length 1
    Code
      foo(x = 1:2)
    Condition
      Error:
      ! <foo> object properties are invalid:
      - @x must be length 1

# prop<- won't infinitly recurse on a custom setter

    Code
      obj <- foo()
    Output
      Starting syncup with value: 
      setting @a <- "a_"
      setting @b <- "b_"
      Starting syncup with value: b_ 
      setting @a <- "a_b_"
      setting @b <- "b_b_"
      Starting syncup with value: 
      setting @a <- "a_"
      Starting syncup with value: a_ 
      setting @a <- "a_a_"
      setting @b <- "b_a_"
      setting @b <- "b_"
    Code
      obj@a <- "val"
    Output
      Starting syncup with value: val 
      setting @a <- "a_val"
      setting @b <- "b_val"
      Starting syncup with value: b_val 
      setting @a <- "a_b_val"
      setting @b <- "b_b_val"

# custom setters can invoke setters on non-self objects

    Code
      receiver <- Receiver()
    Output
      [rx] receiving:  
      [rx] finished receiving.
    Code
      transmitter <- Transmitter()
    Output
      [tx] sending:  
      [rx] receiving:  
      [rx] finished receiving.
      [tx] saving last sent message.
      [tx] finished transmitting.
    Code
      transmitter@message <- "hello"
    Output
      [tx] sending:  hello 
      [rx] receiving:  hello 
      [rx] finished receiving.
      [tx] saving last sent message.
      [tx] finished transmitting.
    Code
      expect_equal(receiver@message, "hello")
      transmitter@message <- "goodbye"
    Output
      [tx] sending:  goodbye 
      [rx] receiving:  goodbye 
      [rx] finished receiving.
      [tx] saving last sent message.
      [tx] finished transmitting.
    Code
      expect_equal(receiver@message, "goodbye")

