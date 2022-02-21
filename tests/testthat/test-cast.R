test_that("can register cast methods", {
  casttest <- new_class("casttest")
  method(cast, list(casttest, character)) <- function(from, to, ...) "c"
  method(cast, list(casttest, integer)) <- function(from, to, ...) "i"

  obj <- casttest()
  expect_equal(cast(obj, character), "c")
  expect_equal(cast(obj, integer), "i")

  # Errors if none found
  expect_snapshot(cast(obj, double), error = TRUE)
})

test_that("doesn't cast to subclass", {
  casttest1 <- new_class("casttest1")
  casttest2 <- new_class("casttest2", casttest1)

  method(cast, list(integer, casttest1)) <- function(from, to, ...) "i"
  expect_error(cast(integer, casttest2), "Can't find method")
})

test_that("fallback casts work", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", foo1)
  expect_equal(class(cast(foo2(), foo1)), c("foo1", "R7_object"))

  factor2 <- new_class("factor2", S3_factor)
  expect_s3_class(cast(factor2(1, "x"), S3_factor), "factor")

  character2 <- new_class("character2", "character")
  expect_equal(cast(character2("x"), character), "x")
})

describe("cast_next()", {
  it("overrides dispatch, matching exact class", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)
    foo3 <- new_class("foo3", foo2)

    bar <- new_generic("bar", "x")
    method(bar, foo1) <- function(x) 1
    method(bar, foo3) <- function(x) 3

    expect_error(bar(cast_next(foo3())), "Can't find method")
    expect_equal(bar(cast_next(foo3(), foo1)), 1)
  })

  it("only affects one dispatch", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    bar1 <- new_generic("bar1", "x")
    method(bar1, foo1) <- function(x) 1
    method(bar1, foo2) <- function(x) 2

    bar2 <- new_generic("bar2", "x")
    method(bar2, foo1) <- function(x) c(1, bar1(x))
    method(bar2, foo2) <- function(x) c(2, bar1(x))

    expect_equal(bar2(cast_next(foo2(), foo1)), c(1, 2))
    expect_equal(bar2(cast(foo2(), foo1)), c(1, 1))
  })

  it("checks to", {
    expect_snapshot(error = TRUE, {
      cast_next(R7_object)
      foo <- new_class("foo")
      cast_next(foo(), character)
    })
  })
})
