
test_that("substitute() works for multiple dispatch method calls like S3", {
  foo <- new_generic("foo", dispatch_args = c("x", "y"))

  new_method(foo, "character", function(x, y, ...) c(substitute(x), substitute(y)))

  bar <- "blah"
  baz <- "bloo"
  expect_equal(foo(bar, baz), c(as.symbol("bar"), as.symbol("baz")))
})


test_that("generics pass ... to methods, and methods can define additional arguments", {
  foo <- new_generic("foo", dispatch_args = "x")

  # base type
  method(foo, "character") <- function(x, sep = "-") paste0("foo", sep, x)
  expect_equal(foo("bar"), "foo-bar")
  expect_equal(foo("bar", sep = "/"), "foo/bar")

  # R7
  method(foo, "text") <- function(x, sep = "-") paste0("foo", sep, x)
  expect_equal(foo(text("bar")), "foo-bar")
  expect_equal(foo(text("bar"), sep = "/"), "foo/bar")
})


test_that("method lookup fails with an informative message for single classes", {
  foo <- new_generic(name="foo", dispatch_args = c("x", "y"))
  method(foo, c("character", "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot_error(
    foo(TRUE, list())
  )

  expect_snapshot_error(
    foo(TRUE)
  )
})

test_that("method lookup fails with an informative message for multiple classes", {
  foo <- new_generic(name="foo", dispatch_args = c("x", "y"))
  method(foo, c("character", "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot_error(
    foo(tibble::tibble(), .POSIXct(double()))
  )
})
