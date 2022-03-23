# checks that input is a class

    Code
      R7_inherits(1:10, "x")
    Error <simpleError>
      `class` is not an <R7_class>

# throws informative error

    Code
      foo1 <- new_class("foo1")
      foo2 <- new_class("foo2")
      check_R7_inherits(foo1(), foo2)
    Error <simpleError>
      `foo1()` must be a <foo2>, not a <foo1>

