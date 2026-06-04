test_that("S7_user_frame() returns the calling frame of the generic", {
  foo <- new_generic("foo", "x")

  x <- 1
  method(foo, class_double) <- function(x) eval(quote(x), S7_user_frame())

  expect_equal(foo(1), 1)
  local({
    x <- 2
    expect_equal(foo(1), 2)
  })

  # Even in the presence of super()
  Number <- new_class("Number", parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))

  expect_equal(foo(Number(1)), 1)
  local({
    x <- 2
    expect_equal(foo(Number(1)), 2)
  })
})

test_that("S7_generic_call() is the originating call to the generic", {
  foo <- new_generic("foo", "x")
  method(foo, class_double) <- function(x) S7_generic_call()
  expect_equal(foo(1), quote(foo(1)))

  # Even in the presence of super()
  Number <- new_class("Number", parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))
  expect_equal(foo(Number(1)), quote(foo(Number(1))))
})

test_that("a different nested generic stops the walk (nearest generic)", {
  inner <- new_generic("inner", "x")
  outer <- new_generic("outer", "x")

  method(inner, class_double) <- function(x) {
    S7_generic_call()
  }
  method(outer, class_double) <- function(x) {
    list(
      inner = inner(x),
      outer = S7_generic_call()
    )
  }
  expect_equal(outer(1), list(inner = quote(inner(x)), outer = quote(outer(1))))
})

test_that("helpers error when called outside a method", {
  expect_snapshot(error = TRUE, {
    S7_generic_call()
    S7_user_frame()
  })
})
