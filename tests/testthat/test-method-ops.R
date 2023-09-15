test_that("Ops generics dispatch to S7 methods", {
  on.exit(S4_remove_classes(c("Foo")))
  FooS4 <- methods::setClass("Foo", contains = "character")

  ClassX <- new_class("ClassX")
  method(`+`, list(class_any, ClassX)) <- function(e1, e2) "class_any + ClassX"
  method(`+`, list(ClassX, class_any)) <- function(e1, e2) "ClassX + class_any"

  expect_equal(ClassX() + ClassX(), "ClassX + class_any")
  expect_equal(ClassX() + 1, "ClassX + class_any")
  expect_equal(1 + ClassX(), "class_any + ClassX")
  expect_equal(ClassX() + FooS4(), "ClassX + class_any")
  expect_equal(FooS4() + ClassX(), "class_any + ClassX")
})

test_that("S7 dispatch beats S3 dispatch in modern R", {
  skip_if(getRversion() < "4.3")

  ClassX <- new_class("ClassX")
  method(`+`, list(class_any, ClassX)) <- function(e1, e2) "class_any + ClassX"
  method(`+`, list(ClassX, class_any)) <- function(e1, e2) "ClassX + class_any"

  expect_equal(ClassX() + factor(), "ClassX + class_any")
  expect_equal(factor() + ClassX(), "class_any + ClassX")
})

test_that("`%*%` dispatches to S7 methods", {
  skip_if(getRversion() < "4.3")

  ClassX <- new_class("ClassX")
  method(`%*%`, list(ClassX, class_any)) <- function(x, y) "ClassX %*% class_any"
  method(`%*%`, list(class_any, ClassX)) <- function(x, y) "class_any %*% ClassX"

  expect_equal(ClassX() %*% ClassX(), "ClassX + class_any")
  expect_equal(ClassX() %*% 1, "ClassX + class_any")
  expect_equal(1 %*% ClassX(), "class_any + ClassX")
})

