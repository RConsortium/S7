# checks that input is a class

    Code
      S7_inherits(1:10, "x")
    Error <simpleError>
      `class` must be an <S7_class> or NULL

# throws informative error

    Code
      foo1 <- new_class("foo1")
      foo2 <- new_class("foo2")
      check_is_S7(foo1(), foo2)
    Error <simpleError>
      `foo1()` must be a <foo2>, not a <foo1>

---

    Code
      check_is_S7("a")
    Error <simpleError>
      `"a"` must be an <S7_object>, not a <character>

