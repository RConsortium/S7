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
  foo <- new_generic("foo", dispatch_args = "x")

  expect_snapshot_error(
    method(foo, list("blah"))
  )
})

test_that("methods can be registered for a generic and then called", {
  foo <- new_generic("foo", dispatch_args = "x")
  new_method(foo, text, function(x, ...) paste0("foo-", r7_data(x)))

  expect_equal(foo(text("bar")), "foo-bar")
})

test_that("single inheritance works when searching for methods", {
  foo2 <- new_generic("foo2", dispatch_args = "x")

  new_method(foo2, "character", function(x, ...) paste0("foo2-", x))

  expect_equal(foo2(text("bar")), "foo2-bar")
})

test_that("direct multiple dispatch works", {
  foo3 <- new_generic("foo3", dispatch_args = c("x", "y"))
  new_method(foo3, list(text, number), function(x, y, ...) paste0(x, y))
  expect_equal(foo3(text("bar"), number(1)), "bar1")
})

test_that("inherited multiple dispatch works", {
  foo4 <- new_generic("foo4", dispatch_args = c("x", "y"))
  new_method(foo4, list("character", "numeric"), function(x, y, ...) paste0(x, ":", y))

  expect_equal(foo4(text("bar"), number(1)), "bar:1")
})

test_that("method dispatch works for S3 objects", {
  foo <- new_generic("foo", dispatch_args = "x")
  obj <- structure("hi", class = "my_s3")
  new_method(foo, s3_class("my_s3"), function(x, ...) paste0("foo-", x))

  expect_equal(foo(obj), "foo-hi")
})

test_that("method dispatch works for S4 objects", {
  skip_if_not(requireNamespace("methods"))

  foo <- new_generic("foo", dispatch_args = "x")

  Range <- setClass("Range", slots = c(start = "numeric", end = "numeric"))
  new_method(foo, Range, function(x, ...) paste0("foo-", x@start, "-", x@end))

  obj <- Range(start = 1, end = 10)
  expect_equal(foo(obj), "foo-1-10")
})

test_that("new_method works if you use R7 class objects", {
  foo5 <- new_generic("foo5", dispatch_args = c("x", "y"))
  new_method(foo5, list(text, number), function(x, y, ...) paste0(x, ":", y))

  expect_equal(foo5(text("bar"), number(1)), "bar:1")
})

test_that("new_method works if you pass a bare class", {
  foo6 <- new_generic("foo6", dispatch_args = "x")
  new_method(foo6, text, function(x, ...) paste0("foo-", x))

  expect_equal(foo6(text("bar")), "foo-bar")
})

test_that("new_method works if you pass a bare class union", {
  foo7 <- new_generic("foo7", dispatch_args = "x")
  new_method(foo7, new_union(text, number), function(x, ...) paste0("foo-", x))

  expect_equal(foo7(text("bar")), "foo-bar")
  expect_equal(foo7(number(1)), "foo-1")
})

test_that("next_method works for single dispatch", {
  foo <- new_generic("foo", dispatch_args = "x")

  new_method(foo, text, function(x, ...) {
    x@.data <- paste0("foo-", r7_data(x))
  })
  new_method(foo, "character", function(x, ...) {
    as.character(x)
  })

  expect_equal(foo(text("hi")), "foo-hi")
})

test_that("next_method works for double dispatch", {
  skip("Currently broken")
  foo <- new_generic("foo", dispatch_args = c("x", "y"))

  new_method(foo, list(text, number), function(x, y, ...) {
    r7_data(x) <- paste0("foo-", r7_data(x), "-", r7_data(y))
    next_method()(x)
  })

  new_method(foo, list(character, number), function(x, y, ...) {
    r7_data(y) <- y + 1
    r7_data(x) <- paste0(r7_data(x), "-", r7_data(y))
    next_method()(x, y)
  })

  new_method(foo, list(character, double), function(x, y, ...) {
    as.character(r7_data(x))
  })

  expect_equal(foo(text("hi"), number(1)), "foo-hi-1-2")
})

test_that("method_compatible returns TRUE if the functions are compatible", {
  foo <- new_generic("foo", function(x, ...) method_call())
  expect_true(method_compatible(function(x, ...) x, foo))
  # extra arguments are ignored
  expect_true(method_compatible(function(x, ..., y) x, foo))

  foo <- new_generic("foo", function(x) method_call())
  expect_true(method_compatible(function(x) x, foo))
})

test_that("method_compatible errors if the functions are not compatible", {
  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", dispatch_args = "x")
    method_compatible(function(y) {}, foo)
    method_compatible(function(x = "foo") {}, foo)
    method_compatible(function(x, y, ...) {}, foo)
  })
})

test_that("method_compatible warn if default arguments don't match", {
  expect_snapshot({
    foo <- new_generic("foo", function(x, ..., z = 2, y = 1) method_call())
    method_compatible(function(x, ..., y = 1) {}, foo)
    method_compatible(function(x, ..., y = 1, z = 1) {}, foo)
  })
})

test_that("R7_method printing", {
  foo <- new_generic(name="foo", dispatch_args = c("x", "y"))
  method(foo, list(text, "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot(
    method(foo, list(text, "integer")),
    transform = scrub_environment
  )
})
