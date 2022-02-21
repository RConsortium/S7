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
