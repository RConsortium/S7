describe("single dispatch", {
  foo <- new_generic("foo", "x")

  it("works for specials", {
    method(foo, class_any) <- function(x) "fallback"
    expect_equal(foo(), "fallback")
    expect_equal(foo(1), "fallback")

    method(foo, class_missing) <- function(x) "missing"
    expect_equal(foo(), "missing")
  })

  it("works for base types", {
    method(foo, class_character) <- function(x) "base"

    expect_equal(foo("bar"), "base")
  })

  it("works for S7 objects", {
    text <- new_class("text", class_character)
    method(foo, text) <- function(x) "S7"

    expect_equal(foo(text("bar")), "S7")
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
    method(foo, new_union(class_integer, class_logical)) <- function(x) "union"

    expect_equal(foo(TRUE), "union")
    expect_equal(foo(1L), "union")
  })
})

describe("multiple dispatch", {
  it("works", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    bar <- new_generic("bar", c("x", "y"))
    method(bar, list(foo1, foo1)) <- function(x, y) c(1, 1)
    method(bar, list(foo2, foo2)) <- function(x, y) c(2, 2)

    expect_equal(bar(foo1(), foo1()), c(1, 1))
    expect_equal(bar(foo1(), foo2()), c(1, 1))
    expect_equal(bar(foo2(), foo1()), c(1, 1))
    expect_equal(bar(foo2(), foo2()), c(2, 2))
  })
})


test_that("can substitute() args", {
  foo <- new_generic("foo", "x", function(x, ..., z = 1) S7_dispatch())
  method(foo, class_character) <- function(x, ..., z = 1) substitute(x)
  expect_equal(foo(letters), quote(letters))

  suppressMessages(
    method(foo, class_character) <- function(x, ..., z = 1, y) substitute(y)
  )
  expect_equal(foo("x", y = letters), quote(letters))

  # Doesn't work currently
  # method(foo, class_character) <- function(x, ..., z = 1) substitute(z)
  # expect_equal(foo("x", z = letters), quote(letters))
})

test_that("methods get values modified in the generic", {
  foo <- new_generic("foo", "x", function(x, y = 1) {
    y <- 10
    S7_dispatch()
  })
  method(foo, class_character) <- function(x, y = 1) y
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
  method(f, class_double) <- function(x) x
  expect_equal(f(counter()), 1)
})

test_that("generics pass ... to methods", {
  foo <- new_generic("foo", "x")

  method(foo, class_character) <- function(x, y = 1) y
  expect_equal(foo("x"), 1)
  expect_equal(foo("x", y = 2), 2)
  expect_snapshot_error(foo("x", z = 2))
})

test_that("generics pass extra args to methods", {
  foo <- new_generic("foo", "x", function(x, ..., z = 1) S7_dispatch())
  method(foo, class_character) <- function(x, ..., z = 1) z
  expect_equal(foo("x", z = 3), 3)
})

test_that("can dispatch on base 'union' types", {
  foo <- new_generic("foo", "x")
  suppressMessages({
    method(foo, class_vector) <- function(x) "v"
    method(foo, class_atomic) <- function(x) "a"
    method(foo, class_numeric) <- function(x) "n"
    method(foo, class_integer) <- function(x) "i"
  })

  expect_equal(foo(list()), "v")
  expect_equal(foo(character()), "a")
  expect_equal(foo(double()), "n")
  expect_equal(foo(integer()), "i")
})

test_that("single dispatch fails with informative messages", {
  fail <- new_generic("fail", "x")

  foo <- new_class("foo")
  Foo <- setClass("Foo", slots = list("x" = "numeric"))
  on.exit(S4_remove_classes("Foo"))

  expect_snapshot(error = TRUE, {
    fail(TRUE)
    fail(tibble::tibble())
    fail(foo())
    fail(Foo(x = 1))
  })
})

test_that("multiple dispatch fails with informative messages", {
  fail <- new_generic("fail", c("x", "y"))

  foo <- new_class("foo")
  Foo <- setClass("Foo", slots = list("x" = "numeric"))
  on.exit(S4_remove_classes("Foo"))

  expect_snapshot(error = TRUE, {
    fail(TRUE)
    fail(, TRUE)
    fail(TRUE, TRUE)
  })
})


test_that("method dispatch preserves method return visibility", {
  foo <- new_generic("foo", "x")
  method(foo, class_integer) <- function(x) invisible("bar")
  expect_invisible(foo(1L))

  method(foo, class_character) <- function(x) {
    if (x == "nope") return(invisible("bar"))
    "bar"
  }

  expect_visible(foo("yep"))
  expect_invisible(foo("nope"))
})

test_that("can dispatch on evaluated arguments", {
  my_generic <- new_generic("my_generic", "x", function(x) {
    x <- 10
    S7_dispatch()
  })
  method(my_generic, class_numeric) <- function(x) 100
  expect_equal(my_generic("x"), 100)
})
