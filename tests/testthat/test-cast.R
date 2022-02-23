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

describe("fallback cast", {
  it("can cast to own class", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    obj <- cast(foo2(), foo2)
    expect_equal(class(obj), c("foo2", "foo1", "R7_object"))
    expect_equal(object_class(obj), foo2)
  })

  it("can cast to super class", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    obj <- cast(foo2(), foo1)
    expect_equal(class(obj), c("foo1", "R7_object"))
    expect_equal(object_class(obj), foo1)
  })

  it("can cast to S3 class", {
    factor2 <- new_class("factor2", S3_factor)
    obj <- cast(factor2(1, "x"), S3_factor)
    expect_equal(class(obj), "factor")
    expect_equal(object_class(factor2), NULL)
  })

  it("can cast to base type", {
    character2 <- new_class("character2", "character")
    obj <- cast(character2("x"), character)
    expect_equal(attr(obj, "class"), NULL)
    expect_equal(object_class(obj), NULL)
  })
})
