describe("method registration", {
  it("checks argument types", {
    foo <- new_generic("foo", dispatch_args = "x")
    expect_snapshot(error = TRUE, {
      x <- 10
      method(x, "character") <- function(x) ...
      method(foo, 1) <- function(x) ...
      method(foo, "character") <- 1
    })
  })
})

test_that("union methods are registered individually", {
  foo <- new_generic("foo", dispatch_args = "x")
  method(foo, new_union(number, "integer")) <- function(x) "x"

  # one method for each union component
  expect_length(methods(foo), 2)
  # and methods printed nicely
  expect_snapshot(foo)
})

test_that("check_method returns TRUE if the functions are compatible", {
  foo <- new_generic("foo", function(x, ...) method_call())
  expect_true(check_method(function(x, ...) x, "character", foo))
  # extra arguments are ignored
  expect_true(check_method(function(x, ..., y) x, "character", foo))

  foo <- new_generic("foo", function(x) method_call())
  expect_true(check_method(function(x) x, "character", foo))
})

test_that("check_method errors if the functions are not compatible", {
  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", dispatch_args = "x")
    check_method(1, "character", foo)
    check_method(function(y) {}, "character", foo)
    check_method(function(x = "foo") {}, "character", foo)
    check_method(function(x, y, ...) {}, "character", foo)
  })
})

test_that("check_method warn if default arguments don't match", {
  expect_snapshot({
    foo <- new_generic("foo", function(x, ..., z = 2, y = 1) method_call())
    check_method(function(x, ..., y = 1) {}, "character", foo)
    check_method(function(x, ..., y = 1, z = 1) {}, "character", foo)
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
