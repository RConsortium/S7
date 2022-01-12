test_that("can substitute() dispatch args", {
  foo <- new_generic("foo", dispatch_args = "x")
  method(foo, "character") <- function(x, ...) substitute(x)
  expect_equal(foo(letters), quote(letters))
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

  method(foo, "character") <- function(x, sep = "-") paste0("foo", sep, x)
  expect_equal(foo("bar"), "foo-bar")
  expect_equal(foo("bar", sep = "/"), "foo/bar")
  expect_snapshot_error(foo("bar", baz = "/"))
})

test_that("method lookup fails with informative messages", {
  foo <- new_generic("foo", dispatch_args = c("x", "y"))
  method(foo, c("character", "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot_error(foo(TRUE))
  expect_snapshot_error(foo(TRUE, list()))
  expect_snapshot_error(foo(tibble::tibble(), .POSIXct(double())))
})
