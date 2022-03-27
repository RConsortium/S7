test_that("has useful print method", {
  expect_snapshot({
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2")
    new_union(foo1, foo2)
  })
})

test_that("can construct from base types", {
  u1 <- new_union(class_character)
  expect_s3_class(u1, "R7_union")
  expect_equal(u1$classes, list(class_character))

  u2 <- new_union(class_character, class_integer)
  expect_s3_class(u2, "R7_union")
  expect_equal(u2$classes, list(class_character, class_integer))
})

test_that("can construct from unions", {
  u1 <- new_union(class_character)
  u2 <- new_union(class_integer)

  u3 <- new_union(u1, u2)
  expect_s3_class(u3, "R7_union")
  expect_equal(u3$classes, list(class_character, class_integer))

  expect_equal(new_union(u1, class_integer), u3)
})

test_that("base unions display as expected", {
  expect_snapshot({
    class_vector
    str(class_vector)
  })
})

test_that("can construct from S3 and S4 classes", {
  S4_union <- methods::setClass("S4_union")
  u <- new_union(class_factor, S4_union)
  expect_equal(u$classes, list(class_factor, getClass("S4_union")))
})

