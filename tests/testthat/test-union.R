test_that("has useful print method", {
  expect_snapshot({
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2")
    new_union(foo1, foo2)
  })
})

test_that("can construct from base types", {
  u1 <- new_union(class_character)
  expect_s3_class(u1, "S7_union")
  expect_equal(u1$classes, list(class_character))

  u2 <- new_union(class_character, class_integer)
  expect_s3_class(u2, "S7_union")
  expect_equal(u2$classes, list(class_character, class_integer))
})

test_that("can construct from unions", {
  u1 <- new_union(class_character)
  u2 <- new_union(class_integer)

  u3 <- new_union(u1, u2)
  expect_s3_class(u3, "S7_union")
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
  on.exit(S4_remove_classes("S4_union"))

  u <- new_union(class_factor, S4_union)
  expect_equal(u$classes, list(class_factor, getClass("S4_union")))
})

test_that("can construct with |", {
  foo <- new_class("foo")
  Foo1 <- setClass("Foo1", slots = list("x" = "numeric"))
  Foo2 <- setClass("Foo2", slots = list("x" = "numeric"))
  Foo3 <- setClassUnion("Foo3", c("Foo1", "Foo2"))
  on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3")))

  expect_equal(class_integer | class_double, class_numeric)
  expect_equal(class_integer | class_numeric, class_numeric)
  expect_equal(class_integer | class_factor, new_union(class_integer, class_factor))
  expect_equal(class_integer | foo, new_union(class_integer, foo))
  expect_equal(class_integer | Foo1, new_union(class_integer, Foo1))
  expect_equal(class_integer | getClass("Foo1"), new_union(class_integer, Foo1))
  expect_equal(class_integer | Foo3, new_union(class_integer, Foo3))
  expect_equal(class_integer | getClass("Foo3"), new_union(class_integer, Foo3))
  expect_equal(class_integer | class_missing, new_union(class_integer, class_missing))
  expect_equal(class_integer | class_any, new_union(class_integer, class_any))
})

test_that("can construct optional union with syntactic sugar", {
  expect_equal(class_integer | NULL, new_union(class_integer, NULL))
  expect_equal(NULL | class_integer, new_union(NULL, class_integer))
})
