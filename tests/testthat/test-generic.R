test_that("normalize_signature errors appropriately", {
  expect_error(normalize_signature(text), "must either be named types")

  expect_error(normalize_signature(list(text)), "must either be named types")
})

test_that("normalize_signature works with unnamed character vectors", {
  expect_equal(normalize_signature(c("x", "y")), alist(x=, y=, ...=))
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
  expect_equal(guess_signature(function(x, y, ...) {}), c("x", "y"))
  expect_equal(guess_signature(function(x, ..., y = 1) {}), "x")
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
