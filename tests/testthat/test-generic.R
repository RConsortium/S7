test_that("new_generic checks its inputs", {
  expect_snapshot(error = TRUE, {
    new_generic(1)
    new_generic("")
    new_generic("foo", 1)
    new_generic("foo", "x", function(x) {})
  })
})

test_that("derived fun always includes ...", {
  g <- new_generic("g", "x")
  expect_equal(names(formals(g)), c("x", "..."))
})

test_that("check_dispatch_args() produces informative errors", {
  expect_snapshot(error = TRUE, {
    check_dispatch_args(1)
    check_dispatch_args(character())
    check_dispatch_args("")
    check_dispatch_args(NA_character_)
    check_dispatch_args(c("x", "x"))
    check_dispatch_args("...")
    check_dispatch_args("x", function(x, y, ...) {})
    check_dispatch_args("y", function(x, ..., y) {})
  })
})

test_that("R7_generic printing", {
  foo1 <- new_generic("foo1", "x")
  text <- new_class("text")

  method(foo1, "character") <- function(x) 1
  method(foo1, text) <- function(x) 2

  foo3 <- new_generic("foo3", c("x", "y", "z"))
  method(foo3, list("character", text, "character")) <- function(x, y, z, ...) 1
  method(foo3, list("character", "integer", "character")) <- function(x, y, z, ...) 2
  method(foo3, list("character", "integer", "logical")) <- function(x, y, z, ...) 3

  expect_snapshot({
    foo1
    foo3
  })
})

test_that("R7_generic printing with long / many arguments", {
  foo <- new_generic("foo", letters)
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
  expect_equal(find_call(1, quote(x)), NULL)
  expect_equal(find_call(quote(f()), quote(x)), NULL)
  expect_equal(find_call(quote(f(a, b, c)), quote(x)), NULL)

  expect_equal(find_call(quote(x(1)), quote(x)), quote(x(1)))
  expect_equal(find_call(quote(y(x(1))), quote(x)), quote(x(1)))
})
