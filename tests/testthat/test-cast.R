test_that("can up_cast to override method", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", foo1)

  bar <- new_generic("bar", "x")
  method(bar, foo1) <- function(x) 1
  method(bar, foo2) <- function(x) 2

  expect_equal(bar(up_cast(foo2(), foo1)), 1)
})
