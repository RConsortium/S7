test_that("new_generic needs fun or signature", {
  expect_snapshot_error(new_generic())
})

test_that("signature overrules derived signature", {
  g <- new_generic("g", function(x, y, ...) method_call())
  expect_equal(g@signature, c("x", "y", "..."))

  g <- new_generic("g", function(x, y, ...) method_call(), signature = "x")
  expect_equal(g@signature, c("x", "..."))
})

test_that("generics pass ... to methods, and methods can define additional arguments on basic types", {
  foo <- new_generic("foo", signature = "x")
  new_method(foo, "character", function(x, sep = "-", ...) paste0("foo", sep, x))

  expect_equal(foo("bar"), "foo-bar")
  expect_equal(foo("bar", sep = "/"), "foo/bar")
})

test_that("generics pass ... to methods, and methods can define additional arguments on R7 objects", {
  foo <- new_generic("foo", signature = "x")
  new_method(foo, "text", function(x, sep = "-", ...) paste0("foo", sep, x))

  expect_equal(foo(text("bar")), "foo-bar")
  expect_equal(foo(text("bar"), sep = "/"), "foo/bar")
})

test_that("guesses signature from required arguments", {
  expect_equal(guess_signature(function() {}), NULL)
  expect_equal(guess_signature(function(x) {}), "x")
  expect_equal(guess_signature(function(x, y) {}), c("x", "y"))
  expect_equal(guess_signature(function(x, y, ...) {}), c("x", "y", "..."))
  expect_equal(guess_signature(function(x, ..., y = 1) {}), c("x", "..."))
})

test_that("check_signature produces informative errors", {
  expect_snapshot(error = TRUE, {
    check_signature(1)
    check_signature(character())
  })
})

test_that("R7_generic printing", {
  foo <- new_generic(name = "foo", signature = c("x", "y", "z"))
  method(foo, list("character", text, "character")) <- function(x, y, z, ...) 1
  method(foo, list("character", "integer", "character")) <- function(x, y, z, ...) 2
  method(foo, list("character", "integer", "logical")) <- function(x, y, z, ...) 3

  expect_snapshot(
    foo
  )
})

test_that("R7_generic printing with long / many arguments", {
  foo <- new_generic(name = "foo", signature = letters)
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
