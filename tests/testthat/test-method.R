test_that("method will fall back to S3 generics if no R7 generic is defined", {
  expect_equal(
    method(print, list(text)),
    base::print.default
  )
})

test_that("method will accept a character vector (#71)", {
  expect_equal(
    method(print, "character"),
    base::print.default
  )
})

test_that("method errors on invalid inputs", {
  expect_snapshot_error(
    method(print, 1)
  )
  expect_snapshot_error(
    method(print, list(1))
  )

  expect_snapshot_error(
    method(print, list(TRUE, FALSE))
  )
})

test_that("method errors if no method is defined for that class", {
  foo <- new_generic("foo", signature = "x")

  expect_snapshot_error(
    method(foo, list("blah"))
  )
})

test_that("methods can be registered for a generic and then called", {
  foo <- new_generic("foo", signature = "x")
  new_method(foo, text, function(x, ...) paste0("foo-", x@.data))

  expect_equal(foo(text("bar")), "foo-bar")
})

test_that("single inheritance works when searching for methods", {
  foo2 <- new_generic("foo2", signature = "x")

  new_method(foo2, "character", function(x, ...) paste0("foo2-", x))

  expect_equal(foo2(text("bar")), "foo2-bar")
})

test_that("direct multiple dispatch works", {
  foo3 <- new_generic("foo3", signature = c("x", "y"))
  new_method(foo3, list(text, number), function(x, y, ...) paste0(x, y))
  expect_equal(foo3(text("bar"), number(1)), "bar1")
})

test_that("inherited multiple dispatch works", {
  foo4 <- new_generic("foo4", signature = c("x", "y"))
  new_method(foo4, list("character", "numeric"), function(x, y, ...) paste0(x, ":", y))

  expect_equal(foo4(text("bar"), number(1)), "bar:1")
})

test_that("method dispatch works for S3 objects", {
  foo <- new_generic("foo", signature = "x")
  obj <- structure("hi", class = "my_s3")
  new_method(foo, s3_class("my_s3"), function(x, ...) paste0("foo-", x))

  expect_equal(foo(obj), "foo-hi")
})

test_that("method dispatch works for S4 objects", {
  skip_if_not(requireNamespace("methods"))

  foo <- new_generic("foo", signature = "x")

  Range <- setClass("Range", slots = c(start = "numeric", end = "numeric"))
  new_method(foo, Range, function(x, ...) paste0("foo-", x@start, "-", x@end))

  obj <- Range(start = 1, end = 10)
  expect_equal(foo(obj), "foo-1-10")
})

test_that("new_method works if you use R7 class objects", {
  foo5 <- new_generic("foo5", signature = c("x", "y"))
  new_method(foo5, list(text, number), function(x, y, ...) paste0(x, ":", y))

  expect_equal(foo5(text("bar"), number(1)), "bar:1")
})

test_that("new_method works if you pass a bare class", {
  foo6 <- new_generic("foo6", signature = "x")
  new_method(foo6, text, function(x, ...) paste0("foo-", x))

  expect_equal(foo6(text("bar")), "foo-bar")
})

test_that("new_method works if you pass a bare class union", {
  foo7 <- new_generic("foo7", signature = "x")
  new_method(foo7, new_union(text, number), function(x, ...) paste0("foo-", x))

  expect_equal(foo7(text("bar")), "foo-bar")
  expect_equal(foo7(number(1)), "foo-1")

  # one method for each union component
  expect_length(methods(foo7), 2)
  # and methods printed nicely
  expect_snapshot(foo7)
})

test_that("next_method works for single dispatch", {
  foo <- new_generic("foo", signature = "x")

  new_method(foo, text, function(x, ...) {
    x@.data <- paste0("foo-", x@.data)
    next_method()(x)
  })

  new_method(foo, "character", function(x, ...) {
    as.character(x)
  })

  expect_equal(foo(text("hi")), "foo-hi")
})

test_that("next_method works for double dispatch", {
  skip("Currently broken")
  foo <- new_generic("foo", signature = c("x", "y"))

  new_method(foo, list(text, number), function(x, y, ...) {
    x@.data <- paste0("foo-", x@.data, "-", y@.data)
    next_method()(x, y)
  })

  new_method(foo, list(character, number), function(x, y, ...) {
    y@.data <- y + 1
    x@.data <- paste0(x@.data, "-", y@.data)
    next_method()(x, y)
  })

  new_method(foo, list(character, double), function(x, y, ...) {
    as.character(x@.data)
  })

  expect_equal(foo(text("hi"), number(1)), "foo-hi-1-2")
})

test_that("substitute() works for single dispatch method calls like S3", {
  foo <- new_generic("foo", signature = "x")

  new_method(foo, character, function(x, ...) substitute(x))

  bar <- "blah"
  expect_equal(foo(bar), as.symbol("bar"))
})

test_that("substitute() works for multiple dispatch method calls like S3", {
  foo <- new_generic("foo", signature = c("x", "y"))

  new_method(foo, "character", function(x, y, ...) c(substitute(x), substitute(y)))

  bar <- "blah"
  baz <- "bloo"
  expect_equal(foo(bar, baz), c(as.symbol("bar"), as.symbol("baz")))
})

test_that("method_compatible returns TRUE if the functions are compatible", {
  foo <- new_generic("foo", signature = "x")

  expect_true(
    method_compatible(
      function(x, ...) x,
      foo
    )
  )

  # extra arguments are ignored
  expect_true(
    method_compatible(
      function(x, y, ...) x,
      foo
    )
  )

  foo <- new_generic("foo", function(x = NULL) method_call())
  expect_true(
    method_compatible(
      function(x = NULL) x,
      foo
    )
  )

  bar <- new_generic("bar", signature = c("x", "y"))
  expect_true(
    method_compatible(
      function(x, y, ...) x,
      bar
    )
  )

  bar <- new_generic("bar", function(x=NULL, y=1, ...) method_call())
  expect_true(
    method_compatible(
      function(x = NULL, y = 1, ...) x,
      bar
    )
  )
})

test_that("method_compatible throws errors if the functions are not compatible", {
  foo <- new_generic("foo", signature = "x")

  # Different argument names
  expect_snapshot_error(
    method_compatible(
      function(y, ...) y,
      foo
    )
  )

  # No dots in method
  expect_snapshot_error(
    method_compatible(
      function(x) x,
      foo
    )
  )

  # Different default values
  expect_snapshot_error(
    method_compatible(
      function(x = "foo", ...) x,
      foo
    )
  )

  bar <- new_generic("bar", signature = c("x", "y"))

  # Arguments in wrong order
  expect_snapshot_error(
    method_compatible(
      function(y, x, ...) x,
      bar
    )
  )

  # No dots in method
  expect_snapshot_error(
    method_compatible(
      function(x, y) x,
      bar
    )
  )

  # Different default values
  expect_snapshot_error(
    method_compatible(
      function(x, y = NULL) x,
      bar
    )
  )
})

test_that("method compatible verifies that if a generic does not have dots the method should not have dots", {
  foo <- new_generic("foo", function(x) method_call())

  expect_true(
    method_compatible(
      function(x) x,
      foo
    )
  )
  expect_snapshot_error(
    method_compatible(
      function(x, ...) x,
      foo
    )
  )
})

test_that("method lookup fails with an informative message for single classes", {
  foo <- new_generic(name="foo", signature = c("x", "y"))
  method(foo, list("character", "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot_error(
    foo(TRUE, list())
  )

  expect_snapshot_error(
    foo(TRUE)
  )
})

test_that("method lookup fails with an informative message for multiple classes", {
  foo <- new_generic(name="foo", signature = c("x", "y"))
  method(foo, list("character", "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot_error(
    foo(tibble::tibble(), .POSIXct(double()))
  )
})

test_that("R7_method printing", {
  foo <- new_generic(name="foo", signature = c("x", "y"))
  method(foo, list(text, "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot(
    method(foo, list(text, "integer")),
    transform = scrub_environment
  )
})
