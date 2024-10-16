# checks that input is a class

    Code
      S7_inherits(1:10, "x")
    Condition
      Error in `S7_inherits()`:
      ! `class` must be an <S7_class> or NULL

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

