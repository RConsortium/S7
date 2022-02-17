test_that("can up_cast to override method", {
  foo1 <- new_class("foo1", character)
  foo2 <- new_class("foo2", foo1)

  bar <- new_generic("bar", "x")
  method(bar, character) <- function(x) 0
  method(bar, foo1) <- function(x) 1
  method(bar, foo2) <- function(x) 2

  expect_equal(bar(up_cast(foo2(), foo1)), 1)
  expect_equal(bar(up_cast(foo2(), character)), 0)
})

test_that("up_cast matches exact class", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", foo1)
  foo3 <- new_class("foo3", foo2)

  bar <- new_generic("bar", "x")
  method(bar, foo1) <- function(x) 1
  method(bar, foo3) <- function(x) 3

  expect_error(bar(up_cast(foo3(), foo2)), "Can't find method")
})
