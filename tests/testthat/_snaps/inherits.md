# checks that input is a class

    Code
      S7_inherits(1:10, "x")
    Condition
      Error:
      ! Can't convert `class` to a valid class. Class specification must be an S7 class object, the result of `new_S3_class()`, an S4 class object, or a base class, not a <character>.

# throws informative error

    Code
      foo1 <- new_class("foo1", package = NULL)
      foo2 <- new_class("foo2", package = NULL)
      check_is_S7(foo1(), foo2)
    Condition
      Error:
      ! `foo1()` must be a <foo2>, not a <foo1>

---

    Code
      check_is_S7("a")
    Condition
      Error:
      ! `"a"` must be an <S7_object>, not a <character>

# check_is_S7() accepts any class specification (#556)

    Code
      check_is_S7(1L, class_character)
    Condition
      Error:
      ! `1L` must be a <character>, not a <integer>
    Code
      check_is_S7(1.5, class_integer | class_character)
    Condition
      Error:
      ! `1.5` must be a <integer> or <character>, not a <double>

