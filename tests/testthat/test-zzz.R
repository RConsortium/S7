test_that("has useful print method", {
  expect_snapshot({
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2")
    new_union(foo1, foo2)
  })
})

test_that("can construct from base types", {
  u1 <- new_union(character)
  expect_s3_class(u1, "R7_union")
  expect_equal(u1@classes, list(base_classes$character))

  u2 <- new_union(character, integer)
  expect_s3_class(u2, "R7_union")
  expect_equal(u2@classes, list(base_classes$character, base_classes$integer))
})

test_that("can construct from unions", {
  u1 <- new_union(character)
  u2 <- new_union(integer)

  u3 <- new_union(u1, u2)
  expect_s3_class(u3, "R7_union")
  expect_equal(u3@classes, list(base_classes$character, base_classes$integer))

  expect_equal(new_union(u1, integer), u3)
})

test_that("base unions print as expected", {
  expect_snapshot({
    base_unions
  })
})

test_that("can construct from S3 and S4 classes", {
  S4_union <- methods::setClass("S4_union")
  u <- new_union(S3_factor, S4_union)
  expect_equal(u@classes, list(S3_factor, getClass("S4_union")))
})

test_that("base classes types check their data", {
  expect_snapshot(error = TRUE, {
    base_classes$integer(TRUE)
  })
})
