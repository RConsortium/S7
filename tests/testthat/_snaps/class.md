# S7 classes / print nicely

    Code
      foo2
    Output
      <foo2> class
      @ parent     : <foo1>
      @ constructor: function(x, y) {...}
      @ validator  : <NULL>
      @ properties :
       $ x: <integer> = integer(0)
       $ y: <integer> = integer(0)
    Code
      str(foo2)
    Output
      <foo2/foo1/S7_object> constructor
       @ name       : chr "foo2"
       @ parent     : <foo1/S7_object> constructor
       @ package    : NULL
       @ properties :List of 2
       .. $ x: <S7_property> 
       ..  ..$ name     : chr "x"
       ..  ..$ class    : <S7_base_class>: <integer>
       ..  ..$ getter   : NULL
       ..  ..$ setter   : NULL
       ..  ..$ validator: NULL
       ..  ..$ default  : NULL
       .. $ y: <S7_property> 
       ..  ..$ name     : chr "y"
       ..  ..$ class    : <S7_base_class>: <integer>
       ..  ..$ getter   : NULL
       ..  ..$ setter   : NULL
       ..  ..$ validator: NULL
       ..  ..$ default  : NULL
       @ abstract   : logi FALSE
       @ constructor: function (x = integer(0), y = integer(0))  
       @ validator  : NULL
    Code
      str(list(foo2))
    Output
      List of 1
       $ : <foo2/foo1/S7_object> constructor

# S7 classes / prints @package and @abstract details

    Code
      foo
    Output
      <S7::foo> abstract class
      @ parent     : <S7_object>
      @ constructor: function() {...}
      @ validator  : <NULL>
      @ properties :

# S7 classes / shows property defaults and read-only annotations

    Code
      Person
    Output
      <S7::Person> class
      @ parent     : <S7_object>
      @ constructor: function(implicit_default, implicit_complex, implicit_S7, default_value, default_expr) {...}
      @ validator  : <NULL>
      @ properties :
       $ implicit_default: <character> = character(0)
       $ implicit_complex: S3<Date>
       $ implicit_S7: <S7::Address> = Address()
       $ default_value: <character> = ""
       $ default_expr: S3<Date> = Sys.Date()
       $ read_only: <ANY> [read-only]

# S7 classes / checks inputs

    Code
      new_class(1)
    Condition
      Error in `new_class()`:
      ! `name` must be a single string.
    Code
      new_class("foo", 1)
    Condition
      Error in `as_class()`:
      ! Can't convert `parent` to a valid class.
      Class specification must be one of the following, not a <double>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class
    Code
      new_class("foo", package = 1)
    Condition
      Error in `new_class()`:
      ! `package` must be a single string.
    Code
      new_class("foo", constructor = 1)
    Condition
      Error in `new_class()`:
      ! `constructor` must be a function.
    Code
      new_class("foo", constructor = function() { })
    Condition
      Error in `new_class()`:
      ! `constructor` must contain a call to `new_object()`.
    Code
      new_class("foo", validator = function() { })
    Condition
      Error in `new_class()`:
      ! `validator` must be function(self), not function().

# S7 classes / can't inherit from S4 or class unions

    Code
      new_class("test", parent = parentS4)
    Condition
      Error in `new_class()`:
      ! `parent` must be an S7 class, S3 class, or base type, not an S4 class.
    Code
      new_class("test", parent = new_union("character"))
    Condition
      Error in `as_class()`:
      ! Can't convert `..1` to a valid class.
      Class specification must be one of the following, not a <character>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class

# abstract classes / can't be instantiated

    Code
      foo := new_class(abstract = TRUE)
      foo()
    Condition
      Error in `S7::new_object()`:
      ! Can't construct an object from abstract class <foo>.

# abstract classes / can't inherit from concrete class

    Code
      foo1 := new_class()
      new_class("foo2", parent = foo1, abstract = TRUE)
    Condition
      Error in `new_class()`:
      ! Abstract classes must have abstract parents.

# abstract classes / can use inherited validator from abstract class

    Code
      foo2(x = 2)
    Condition
      Error in `foo2()`:
      ! <foo2> object is invalid:
      - @x has bad value

# new_object() / gives useful error if called directly

    Code
      new_object()
    Condition
      Error in `new_object()`:
      ! `new_object()` must be called from within a constructor.

# new_object() / errors if `.parent` doesn't inherit from the parent class (#409)

    Code
      Foo()
    Condition
      Error in `new_object()`:
      ! `.parent` must be an instance of <Bar>, not S3<S7_base_class>.
    Code
      Baz()
    Condition
      Error in `new_object()`:
      ! `.parent` must be an instance of <integer>, not <character>.

# new_object() / errors if `.parent` is supplied but class has no parent

    Code
      NoParent()
    Condition
      Error in `new_object()`:
      ! `.parent` must not be supplied when class has no parent.

# new_object() / validates object

    Code
      foo("x")
    Condition
      Error in `foo()`:
      ! <foo> object properties are invalid:
      - @x must be <double>, not <character>
    Code
      foo(-1)
    Condition
      Error in `foo()`:
      ! <foo> object is invalid:
      - x must be positive

# new_object() / runs each parent validator exactly once

    Code
      . <- A()
    Output
      A 
    Code
      . <- B()
    Output
      A B 
    Code
      . <- C()
    Output
      A B C 

# S7 object / displays nicely

    Code
      foo := new_class(properties = list(x = class_double, y = class_double),
      package = NULL)
      foo()
    Output
      <foo>
       @ x: num(0) 
       @ y: num(0) 
    Code
      str(list(foo()))
    Output
      List of 1
       $ : <foo>
        ..@ x: num(0) 
        ..@ y: num(0) 

# S7 object / displays objects with data nicely

    Code
      text := new_class(class_character, package = NULL)
      text("x")
    Output
      <text> chr "x"
    Code
      str(list(text("x")))
    Output
      List of 1
       $ : <text> chr "x"

# S7 object / displays data.frame subclasses without error (#494)

    Code
      str(mydf(data.frame(a = 1:2, b = 1:2)))
    Output
      Classes 'mydf', 'S7_object' and 'data.frame':	2 obs. of  2 variables:
      <mydf> 'data.frame':	2 obs. of  2 variables:
       $ a: int  1 2
       $ b: int  1 2

# S7 object / displays list objects nicely

    Code
      foo1(list(x = 1, y = list(a = 21, b = 22)), x = 3, y = list(a = 41, b = 42))
    Output
      <foo1> List of 2
       $ x: num 1
       $ y:List of 2
        ..$ a: num 21
        ..$ b: num 22
       @ x: num 3
       @ y:List of 2
       .. $ a: num 41
       .. $ b: num 42

# c(<S7_class>, ...) gives error

    Code
      c(foo1, foo1)
    Condition
      Error in `c.S7_class()`:
      ! Can not combine S7 class objects.

# can't create class with `...` property name

    Code
      new_class("foo", properties = list(... = class_character))
    Condition
      Error in `new_class()`:
      ! Properties can't be named "...".

# S7_class() gives informative error if no S7 spec available

    Code
      S7_class(pairlist(x = 1))
    Condition
      Error:
      ! No S7 class for base type <pairlist>.

