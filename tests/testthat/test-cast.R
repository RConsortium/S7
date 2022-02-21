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

test_that("fallback casts work", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", foo1)
  expect_equal(class(cast(foo2(), foo1)), c("foo1", "R7_object"))

  # TODO: get this working
  # factor2 <- new_class("factor2", S3_factor)
  # expect_s3_class(cast(factor2(1, "x"), S3_factor), "factor")

  character2 <- new_class("character2", "character")
  expect_equal(cast(character2("x"), character), "x")
})
