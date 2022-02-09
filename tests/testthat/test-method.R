
describe("single dispatch", {
  foo <- new_generic("foo", dispatch_args = "x")

  it("works for base types", {
    method(foo, "character") <- function(x) "base"

    expect_equal(foo("bar"), "base")
  })

  it("works for R7 objects", {
    method(foo, text) <- function(x) "R7"

    expect_equal(foo(text("bar")), "R7")
  })

  it("works for S3 objects", {
    obj <- structure("hi", class = "my_s3")
    method(foo, s3_class("my_s3")) <- function(x) "S3"

    expect_equal(foo(obj), "S3")
  })

  it("works for S4 objects", {
    my_S4 <- setClass("my_S4", contains = "numeric")
    method(foo, my_S4) <- function(x) "S4"

    expect_equal(foo(my_S4(1)), "S4")
  })

  it("works for unions", {
    method(foo, new_union(number, "integer")) <- function(x) "union"

    expect_equal(foo(number(1)), "union")
    expect_equal(foo(1L), "union")
  })
})

describe("multiple dispatch", {
  it("works directly", {
    foo <- new_generic("foo3", dispatch_args = c("x", "y"))
    method(foo, list(text, number)) <- function(x, y) paste0(x, y)
    expect_equal(foo(text("bar"), number(1)), "bar1")
  })

  it("works via inheritance", {
    foo <- new_generic("foo", dispatch_args = c("x", "y"))
    method(foo, list("character", "numeric")) <- function(x, y) paste0(x, ":", y)

    expect_equal(foo(text("bar"), number(1)), "bar:1")
  })
})

test_that("union methods are created individually", {
  foo <- new_generic("foo", dispatch_args = "x")

  method(foo, new_union(number, "integer")) <- function(x) "x"

  # one method for each union component
  expect_length(methods(foo), 2)
  # and methods printed nicely
  expect_snapshot(foo)
})

test_that("next_method works for single dispatch", {
  foo <- new_generic("foo", dispatch_args = "x")

  method(foo, text) <- function(x, ...) {
    x@.data <- paste0("foo-", r7_data(x))
  }
  method(foo, "character") <- function(x, ...) {
    as.character(x)
  }

  expect_equal(foo(text("hi")), "foo-hi")
})

test_that("next_method works for double dispatch", {
  foo <- new_generic("foo", dispatch_args = c("x", "y"))

  method(foo, list(text, number)) <- function(x, y, ...) {
    r7_data(x) <- paste0("foo-", r7_data(x), "-", r7_data(y))
    next_method()(x, y)
  }

  method(foo, list(character, number)) <- function(x, y, ...) {
    r7_data(y) <- y + 1
    r7_data(x) <- paste0(r7_data(x), "-", r7_data(y))
    next_method()(x, y)
  }

  method(foo, list(character, double)) <- function(x, y, ...) {
    as.character(r7_data(x))
  }

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
