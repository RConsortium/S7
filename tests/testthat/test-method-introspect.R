describe("method introspection", {
  test_that("can dispatch by class or object", {
    foo <- new_generic("foo", "x")
    method(foo, "character") <- function(x) "c"

    expect_equal(
      method(foo, class = "character"),
      method(foo, object = "x")
    )
  })

  it("errors on invalid inputs", {
    expect_snapshot(error = TRUE, {
      method(print, 1)

      foo <- new_generic("foo", "x")
      method(foo)
      method(foo, 1)
      method(foo, new_union("integer", "double"))

      foo2 <- new_generic("foo2", c("x", "y"))
      method(foo2, object = list("character"))
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
