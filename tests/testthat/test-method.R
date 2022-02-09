test_that("union methods are created individually", {
  foo <- new_generic("foo", dispatch_args = "x")

  method(foo, new_union(number, "integer")) <- function(x) "x"

  # one method for each union component
  expect_length(methods(foo), 2)
  # and methods printed nicely
  expect_snapshot(foo)
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
