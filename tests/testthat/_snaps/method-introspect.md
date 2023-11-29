# method introspection: errors on invalid inputs

    Code
      method(print, 1)
    Condition
      Error:
      ! `generic` must be a <S7_generic>, not a <closure>
    Code
      foo <- new_generic("foo", "x")
      method(foo)
    Condition
      Error:
      ! Must supply exactly one of `class` and `object`
    Code
      method(foo, 1)
    Condition
      Error:
      ! Can't convert `signature` to a valid class. Class specification must be an S7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <double>.
    Code
      method(foo, new_union(class_integer, class_double))
    Condition
      Error in `as_dispatch()`:
      ! Can't dispatch on unions; must be a concrete type
    Code
      foo2 <- new_generic("foo2", c("x", "y"))
      method(foo2, object = list(class_character))
    Condition
      Error:
      ! `object` must be length 2

# method introspection: errors if no method found

    Code
      method(foo, class = class_integer)
    Condition
      Error:
      ! Can't find method for `foo(<integer>)`.
    Code
      method(foo, object = 1L)
    Condition
      Error:
      ! Can't find method for `foo(<integer>)`.
    Code
      method(foo2, class = list(class_integer, class_double))
    Condition
      Error:
      ! Can't find method for generic `foo(x, y)`:
      - x: <integer>
      - y: <double>
    Code
      method(foo2, object = list(1L, 2))
    Condition
      Error:
      ! Can't find method for generic `foo(x, y)`:
      - x: <integer>
      - y: <double>

# method explanation: shows all possible methods along with matches

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

