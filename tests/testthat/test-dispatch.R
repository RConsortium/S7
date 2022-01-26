test_that("can substitute() args", {
  foo <- new_generic("foo", function(x, ..., z = 1) method_call())
  method(foo, "character") <- function(x, ..., z = 1) substitute(x)
  expect_equal(foo(letters), quote(letters))

  method(foo, "character") <- function(x, ..., z = 1, y) substitute(y)
  expect_equal(foo("x", y = letters), quote(letters))

  # Doesn't work currently
  # method(foo, "character") <- function(x, ..., z = 1) substitute(z)
  # expect_equal(foo("x", z = letters), quote(letters))
})

test_that("methods get values modified in the generic", {
  foo <- new_generic("foo", function(x, y = 1) {
    y <- 10
    method_call()
  })
  method(foo, "character") <- function(x, y = 1) y
  expect_equal(foo("x", 1), 10)
})

test_that("dispatched arguments are evaluated once", {
  counter <- local({
    i <- 0
    function() {
      i <<- i + 1
      i
    }
  })

  f <- new_generic("f", dispatch_args = "x")
  method(f, "numeric") <- function(x) x
  expect_equal(f(counter()), 1)
})

test_that("generics pass ... to methods", {
  foo <- new_generic("foo", dispatch_args = "x")

  method(foo, "character") <- function(x, y = 1) y
  expect_equal(foo("x"), 1)
  expect_equal(foo("x", y = 2), 2)
  expect_snapshot_error(foo("x", z = 2))
})

test_that("generics pass extra args to methods", {
  foo <- new_generic("foo", function(x, ..., z = 1) method_call())
  method(foo, "character") <- function(x, ..., z = 1) z
  expect_equal(foo("x", z = 3), 3)
})

test_that("method lookup fails with informative messages", {
  foo <- new_generic("foo", dispatch_args = c("x", "y"))
  method(foo, c("character", "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot_error(foo(TRUE))
  expect_snapshot_error(foo(TRUE, list()))
  expect_snapshot_error(foo(tibble::tibble(), .POSIXct(double())))
})
