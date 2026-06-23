# method introspection errors on invalid inputs

    Code
      method(print, 1)
    Condition
      Error in `method()`:
      ! `generic` must be a <S7_generic>, not a <closure>.
    Code
      foo := new_generic("x")
      method(foo)
    Condition
      Error in `method()`:
      ! Must supply exactly one of `class` and `object`.
    Code
      method(foo, 1)
    Condition
      Error in `as_class()`:
      ! Can't convert `signature` to a valid class.
      Class specification must be one of the following, not a <double>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class
    Code
      method(foo, new_union(class_integer, class_double))
    Condition
      Error in `method()`:
      ! Can't dispatch on unions; must be a concrete type.
    Code
      foo2 := new_generic(c("x", "y"))
      method(foo2, object = list(class_character))
    Condition
      Error in `method()`:
      ! `object` must be length 2.

# method introspection errors if no method found

    Code
      method(foo, class = class_integer)
    Condition
      Error in `method()`:
      ! Can't find method for `foo(<integer>)`.
    Code
      method(foo, object = 1L)
    Condition
      Error in `method()`:
      ! Can't find method for `foo(<integer>)`.
    Code
      method(foo2, class = list(class_integer, class_double))
    Condition
      Error in `method()`:
      ! Can't find method for generic `foo2(x, y)`:
      - x: <integer>
      - y: <double>
    Code
      method(foo2, object = list(1L, 2))
    Condition
      Error in `method()`:
      ! Can't find method for generic `foo2(x, y)`:
      - x: <integer>
      - y: <double>

# method introspection requires external class's package to be loaded

    Code
      method(foo, class = Ext)
    Condition
      Error:
      ! Can't find external class <not_a_package::Ext>:
      * Package 'not_a_package' is not installed.
    Code
      method_explain(foo, class = Ext)
    Condition
      Error:
      ! Can't find external class <not_a_package::Ext>:
      * Package 'not_a_package' is not installed.

# method explanation shows all possible methods along with matches

       add([foo2], [foo2])
    -> add([foo2], [foo1])
       add([foo2], [S7_object])
       add([foo2], [ANY])
       add([foo1], [foo2])
    *  add([foo1], [foo1])
       add([foo1], [S7_object])
       add([foo1], [ANY])
       add([S7_object], [foo2])
       add([S7_object], [foo1])
       add([S7_object], [S7_object])
       add([S7_object], [ANY])
       add([ANY], [foo2])
       add([ANY], [foo1])
       add([ANY], [S7_object])
       add([ANY], [ANY])

