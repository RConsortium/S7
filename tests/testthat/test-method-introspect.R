describe("method introspection", {
  test_that("can dispatch by class or object", {
    foo <- new_generic("foo", "x")
    method(foo, class_character) <- function(x) "c"

    expect_equal(
      method(foo, class = class_character),
      method(foo, object = "x")
    )
  })

  it("errors on invalid inputs", {
    expect_snapshot(error = TRUE, {
      method(print, 1)

      foo <- new_generic("foo", "x")
      method(foo)
      method(foo, 1)
      method(foo, new_union(class_integer, class_double))

      foo2 <- new_generic("foo2", c("x", "y"))
      method(foo2, object = list(class_character))
    })
  })

  test_that("errors if no method found", {
    foo <- new_generic("foo", "x")

    expect_snapshot(error = TRUE, {
      method(foo, list())
      method(foo, list("blah"))
    })
  })
})

describe("method explanation", {
  it("shows all possible methods along with matches", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    add <- new_generic("add", c("x", "y"))
    method(add, list(foo2, foo1)) <- function(x, y) c(2, 1)
    method(add, list(foo1, foo1)) <- function(x, y) c(1, 1)

    expect_snapshot_output(method_explain(add, list(foo2, foo2)))
  })
})
