test_that("can work with classGenerators", {
  Foo <- setClass("Foo")
  expect_equal(S4_to_R7_class(Foo), getClass("Foo"))
})

test_that("converts S4 base classes to R7 base classes", {
  expect_equal(S4_to_R7_class(getClass("NULL")), base_classes[["NULL"]])
  expect_equal(S4_to_R7_class(getClass("character")), base_classes$character)
})

test_that("converts S4 unions to R7 unions", {
  setClass("Foo1", slots = "x")
  setClass("Foo2", slots = "x")

  setClassUnion("Union1", c("Foo1", "Foo2"))
  expect_equal(
    S4_to_R7_class(getClass("Union1")),
    new_union(getClass("Foo1"), getClass("Foo2"))
  )

  setClass("Foo3", slots = "x")
  setClassUnion("Union2", c("Union1", "Foo3"))
  expect_equal(
    S4_to_R7_class(getClass("Union2")),
    new_union(getClass("Foo1"), getClass("Foo2"), getClass("Foo3"))
  )
})

test_that("converts S4 representation of S3 classes to R7 representation", {
  expect_equal(S4_to_R7_class(getClass("Date")), new_S3_class("Date"), ignore_function_env = TRUE)
})

test_that("errors on non-S4 classes", {
  expect_snapshot(S4_to_R7_class(1), error = TRUE)
})
