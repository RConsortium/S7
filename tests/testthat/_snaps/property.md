# property retrieval / retrieves the properties that exist & errors otherwise

    Code
      prop(obj, "x")
    Condition
      Error in `<foo>@x`:
      ! Property not found.
    Code
      obj@x
    Condition
      Error in `<foo>@x`:
      ! Property not found.

# prop setting / can't set read-only properties

    Code
      obj@x <- 1
    Condition
      Error in `<foo>@x`:
      ! Can't set read-only property.

# prop setting / errors if the property doesn't exist or is wrong class

    Code
      obj <- foo(123)
      obj@foo <- 10
    Condition
      Error in `<foo>@foo`:
      ! Property not found.
    Code
      obj@x <- "x"
    Condition
      Error in `<foo>@x`:
      ! <foo>@x must be <double>, not <character>

# prop setting / validates all attributes if custom setter

    Code
      obj <- foo(y = 123, x = 123)
      obj@x <- "x"
    Condition
      Error in `<foo>@y`:
      ! <foo>@y must be <double>, not <character>

# prop setting / gives informative error if setter doesn't return an S7 object (#416)

    Code
      foo(x = 1.1)
    Condition
      Error in `<foo>@x`:
      ! Custom setter must return an <S7_object>, not <integer>.

# props<- / `check = FALSE` skip validation

    Code
      validate(obj)
    Condition
      Error in `validate()`:
      ! <S7::foo> object is invalid:
      - bad

# props<- / set_props() errors if single unnamed list has unnamed elements (#497)

    Code
      set_props(foo(1), list(2))
    Condition
      Error in `set_props()`:
      ! All elements of `..1` must be named.

# props<- / set_props() skip validation with `.check = FALSE`

    Code
      validate(obj2)
    Condition
      Error in `validate()`:
      ! <S7::foo> object is invalid:
      - bad

# new_property() / validates getter and settor

    Code
      new_property(getter = function(x) { })
    Condition
      Error in `new_property()`:
      ! `getter` must be function(self), not function(x).
    Code
      new_property(setter = function(x, y, z) { })
    Condition
      Error in `new_property()`:
      ! `setter` must be function(self, value) or function(self, name, value), not function(x, y, z).

# new_property() / validates default

    Code
      new_property(class_integer, default = "x")
    Condition
      Error in `new_property()`:
      ! `default` must be an instance of <integer>, not a <character>.

# new_property() / displays nicely

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
       $ null: <NULL> = NULL
       $ base: <integer> = integer(0)
       $ S3: S3<factor>
       $ S4: S4<class_S4> = class_S4()
       $ S7: <class_S7> = class_S7()
       $ S7_union: <integer> or <logical> = integer(0)

---

    Code
      my_obj@null <- "x"
    Condition
      Error in `<my_class>@null`:
      ! <my_class>@null must be <NULL>, not <character>
    Code
      my_obj@base <- "x"
    Condition
      Error in `<my_class>@base`:
      ! <my_class>@base must be <integer>, not <character>
    Code
      my_obj@S3 <- "x"
    Condition
      Error in `<my_class>@S3`:
      ! <my_class>@S3 must be S3<factor>, not <character>
    Code
      my_obj@S4 <- "x"
    Condition
      Error in `<my_class>@S4`:
      ! <my_class>@S4 must be S4<class_S4>, not <character>
    Code
      my_obj@S7 <- "x"
    Condition
      Error in `<my_class>@S7`:
      ! <my_class>@S7 must be <class_S7>, not <character>
    Code
      my_obj@S7_union <- "x"
    Condition
      Error in `<my_class>@S7_union`:
      ! <my_class>@S7_union must be <integer> or <logical>, not <character>

# as_properties() gives useful error messages

    Code
      as_properties(1)
    Condition
      Error:
      ! `properties` must be a list.
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
      Error in `as_class()`:
      ! Can't convert `property$x` to a valid class.
      Class specification must be one of the following, not a <double>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class
    Code
      as_properties(list(x = class_character, x = class_character))
    Condition
      Error:
      ! `properties` names must be unique.

# can validate with custom validator

    Code
      f <- foo(x = 1L)
      f@x <- 1:2
    Condition
      Error in `<foo>@x`:
      ! <foo>@x must be length 1
    Code
      foo(x = 1:2)
    Condition
      Error in `foo()`:
      ! <foo> object properties are invalid:
      - @x must be length 1

# property validation runs the class's own validator

    Code
      Foo(x = bad)
    Condition
      Error in `Foo()`:
      ! <Foo> object properties are invalid:
      - @x: Not enough 'levels' for underlying data

# property validation runs an S4 class's validity method

    Code
      Foo(x = bad)
    Condition
      Error in `Foo()`:
      ! <Foo> object properties are invalid:
      - @x: n must be positive

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

