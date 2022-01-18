test_that("new_generic needs fun or dispatch_args", {
  expect_snapshot_error(new_generic())
})

test_that("dispatch_args overrules derived", {
  g <- new_generic("g", function(x, y, ...) method_call())
  expect_equal(g@dispatch_args, c("x", "y"))

  g <- new_generic("g", function(x, ...) method_call(), dispatch_args = "x")
  expect_equal(g@dispatch_args, "x")
})

test_that("derived fun always includes ...", {
  g <- new_generic("g", dispatch_args = "x")
  expect_equal(names(formals(g)), c("x", "..."))
})

test_that("guesses dispatch_args from args after dots arguments", {
  expect_equal(guess_dispatch_args(function() {}), character())
  expect_equal(guess_dispatch_args(function(x) {}), "x")
  expect_equal(guess_dispatch_args(function(x, y) {}), "x")

  expect_equal(guess_dispatch_args(function(...) {}), character())
  expect_equal(guess_dispatch_args(function(x, y, ...) {}), c("x", "y"))
  expect_equal(guess_dispatch_args(function(x, ..., y = 1) {}), "x")
})

test_that("check_dispatch_args() produces informative errors", {
  expect_snapshot(error = TRUE, {
    check_dispatch_args(1)
    check_dispatch_args(character())
    check_dispatch_args("...")
    check_dispatch_args("x", function(x, y, ...) {})
    check_dispatch_args("y", function(x, ..., y) {})
  })
})

test_that("R7_generic printing", {
  foo <- new_generic(name = "foo", dispatch_args = c("x", "y", "z"))
  method(foo, list("character", text, "character")) <- function(x, y, z, ...) 1
  method(foo, list("character", "integer", "character")) <- function(x, y, z, ...) 2
  method(foo, list("character", "integer", "logical")) <- function(x, y, z, ...) 3

  expect_snapshot(
    foo
  )
})

test_that("R7_generic printing with long / many arguments", {
  foo <- new_generic(name = "foo", dispatch_args = letters)
  expect_snapshot(
    foo
  )
})


# check_generic_fun -------------------------------------------------------

test_that("check_generic produces informative errors", {
  expect_snapshot(error = TRUE,{
    check_generic("x")
    check_generic(function() {})
  })
})

test_that("has_fun handles expected cases", {
  expect_false(has_call(1, quote(x)))
  expect_false(has_call(quote(f()), quote(x)))
  expect_false(has_call(quote(f(a, b, c)), quote(x)))

  expect_true(has_call(quote(x()), quote(x)))
  expect_true(has_call(quote(y(x())), quote(x)))
})
