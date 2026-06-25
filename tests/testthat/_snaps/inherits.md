# S4 class checks reject spoofed S3 objects

    Code
      holder(x = spoof)
    Condition
      Error in `holder()`:
      ! <holder> object properties are invalid:
      - @x must be S4<Foo>, not S3<Foo>

# checks that input is a class

    Code
      S7_inherits(1:10, "x")
    Condition
      Error in `as_class()`:
      ! Can't convert `class` to a valid class.
      Class specification must be one of the following, not a <character>:
       * An S7 class object
       * An S3 class object (from `new_S3_class()`)
       * An S4 class object
       * A base class

# throws informative error

    Code
      foo1 := new_class(package = NULL)
      foo2 := new_class(package = NULL)
      check_is_S7(foo1(), foo2)
    Condition
      Error:
      ! `foo1()` must be a <foo2>, not a <foo1>.

---

    Code
      check_is_S7("a")
    Condition
      Error:
      ! `"a"` must be an <S7_object>, not a <character>.

# check_is_S7() accepts any class specification (#556)

    Code
      check_is_S7(1L, class_character)
    Condition
      Error:
      ! `1L` must be a <character>, not a <integer>.
    Code
      check_is_S7(1.5, class_integer | class_character)
    Condition
      Error:
      ! `1.5` must be a <integer> or <character>, not a <double>.

