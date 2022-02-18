describe("single dispatch", {
  foo <- new_generic("foo", "x")

  it("works for specials", {
    method(foo, missing_class) <- function(x) "missing"
    method(foo, any_class) <- function(x) "fallback"

    expect_equal(foo(), "missing")
    expect_equal(foo(1), "fallback")
  })

  it("works for base types", {
    method(foo, "character") <- function(x) "base"

    expect_equal(foo("bar"), "base")
  })

  it("works for R7 objects", {
    method(foo, text) <- function(x) "R7"

    expect_equal(foo(text("bar")), "R7")
  })

  it("works for S3 objects", {
    obj <- structure("hi", class = "my_S3")
    method(foo, new_S3_class("my_S3")) <- function(x) "S3"

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
    foo <- new_generic("foo3", c("x", "y"))
    method(foo, list(text, number)) <- function(x, y) paste0(x, y)
    expect_equal(foo(text("bar"), number(1)), "bar1")
  })

  it("works via inheritance", {
    foo <- new_generic("foo", c("x", "y"))
    method(foo, list("character", "numeric")) <- function(x, y) paste0(x, ":", y)

    expect_equal(foo(text("bar"), number(1)), "bar:1")
  })
})


test_that("can substitute() args", {
  foo <- new_generic("foo", "x", function(x, ..., z = 1) method_call())
  method(foo, "character") <- function(x, ..., z = 1) substitute(x)
  expect_equal(foo(letters), quote(letters))

  method(foo, "character") <- function(x, ..., z = 1, y) substitute(y)
  expect_equal(foo("x", y = letters), quote(letters))

  # Doesn't work currently
  # method(foo, "character") <- function(x, ..., z = 1) substitute(z)
  # expect_equal(foo("x", z = letters), quote(letters))
})

test_that("methods get values modified in the generic", {
  foo <- new_generic("foo", "x", function(x, y = 1) {
    y <- 10
    method_call()
  })
  method(foo, "character") <- function(x, y = 1) y
  expect_equal(foo("x", 1), 10)
})

test_that("dispatched arguments are evaluated once", {
  counter <- local({
    i <- 0
    function() {
      i <<- i + 1
      i
    }
  })

  f <- new_generic("f", "x")
  method(f, "numeric") <- function(x) x
  expect_equal(f(counter()), 1)
})

test_that("generics pass ... to methods", {
  foo <- new_generic("foo", "x")

  method(foo, "character") <- function(x, y = 1) y
  expect_equal(foo("x"), 1)
  expect_equal(foo("x", y = 2), 2)
  expect_snapshot_error(foo("x", z = 2))
})

test_that("generics pass extra args to methods", {
  foo <- new_generic("foo", "x", function(x, ..., z = 1) method_call())
  method(foo, "character") <- function(x, ..., z = 1) z
  expect_equal(foo("x", z = 3), 3)
})

test_that("can dispatch on base 'union' types", {
  foo <- new_generic("foo", "x")
  method(foo, "vector") <- function(x) "v"
  method(foo, "atomic") <- function(x) "a"
  method(foo, "numeric") <- function(x) "n"
  method(foo, "integer") <- function(x) "i"

  expect_equal(foo(list()), "v")
  expect_equal(foo(character()), "a")
  expect_equal(foo(double()), "n")
  expect_equal(foo(integer()), "i")
})

test_that("method lookup fails with informative messages", {
  foo <- new_generic("foo", c("x", "y"))
  method(foo, list("character", "integer")) <- function(x, y) paste0("bar:", x, y)
  expect_snapshot_error(foo(TRUE))
  expect_snapshot_error(foo(TRUE, list()))
  expect_snapshot_error(foo(tibble::tibble(), .POSIXct(double())))
})

test_that("next_method works for single dispatch", {
  foo <- new_generic("foo", "x")

  method(foo, text) <- function(x, ...) {
    R7_data(x) <- paste0("foo-", R7_data(x))
  }
  method(foo, "character") <- function(x, ...) {
    as.character(x)
  }

  expect_equal(foo(text("hi")), "foo-hi")
})

test_that("next_method works for double dispatch", {
  foo <- new_generic("foo", c("x", "y"))

  method(foo, list(text, number)) <- function(x, y, ...) {
    R7_data(x) <- paste0("foo-", R7_data(x), "-", R7_data(y))
    next_method()(x, y)
  }

  method(foo, list(character, number)) <- function(x, y, ...) {
    R7_data(y) <- y + 1
    R7_data(x) <- paste0(R7_data(x), "-", R7_data(y))
    next_method()(x, y)
  }

  method(foo, list(character, double)) <- function(x, y, ...) {
    as.character(R7_data(x))
  }

  expect_equal(foo(text("hi"), number(1)), "foo-hi-1-2")
})
