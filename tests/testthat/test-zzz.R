test_that("has useful print method", {
  expect_snapshot({
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2")
    new_union(foo1, foo2)
  })
})

test_that("can construct from base types", {
  expect_equal(
    class_names(new_union(character)),
    c("character", "R7_object")
  )
  expect_equal(
    class_names(new_union(character, integer)),
    c("character", "integer","R7_object")
  )
})

test_that("can construct from unions", {
  u1 <- new_union(character)
  u2 <- new_union(integer)

  expect_equal(
    class_names(new_union(u1, u2)),
    c("character", "integer", "R7_object")
  )
  expect_equal(
    class_names(new_union(u1, integer)),
    c("character", "integer", "R7_object")
  )
})

test_that("base unions print as expected", {
  expect_snapshot({
    base_unions
  })
})

test_that("can construct from S3 and S4 classes", {
  factor <- s3_class("factor")
  s4_union <- methods::setClass("s4_union")
  u <- new_union(factor, s4_union)
  expect_equal(u@classes, list(factor, getClass("s4_union")))
})

test_that("base_classes can construct by default", {
  for (class in base_classes) {
    expect_error(class(), NA)
  }
})
