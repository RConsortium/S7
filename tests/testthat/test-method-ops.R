test_that("Ops generics dispatch to S7 methods for S7 classes", {
  local_methods(base_ops[["+"]])
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2")

  method(`+`, list(foo1, foo1)) <- function(e1, e2) "foo1-foo1"
  method(`+`, list(foo1, foo2)) <- function(e1, e2) "foo1-foo2"
  method(`+`, list(foo2, foo1)) <- function(e1, e2) "foo2-foo1"
  method(`+`, list(foo2, foo2)) <- function(e1, e2) "foo2-foo2"

  expect_equal(foo1() + foo1(), "foo1-foo1")
  expect_equal(foo1() + foo2(), "foo1-foo2")
  expect_equal(foo2() + foo1(), "foo2-foo1")
  expect_equal(foo2() + foo2(), "foo2-foo2")
})

test_that("Ops generics dispatch to S3 methods", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])

  foo <- new_class("foo")
  method(`+`, list(class_factor, foo)) <- function(e1, e2) "factor-foo"
  method(`+`, list(foo, class_factor)) <- function(e1, e2) "foo-factor"

  expect_equal(foo() + factor(), "foo-factor")
  expect_equal(factor() + foo(), "factor-foo")

  # Even if custom method exists
  foo_S3 <- structure(list(), class = "foo_S3")
  assign("+.foo_S3", function(e1, e2) stop("Failure!"), envir = globalenv())
  defer(rm("+.foo_S3", envir = globalenv()))

  method(`+`, list(new_S3_class("foo_S3"), foo)) <- function(e1, e2) "S3-S7"
  method(`+`, list(foo, new_S3_class("foo_S3"))) <- function(e1, e2) "S7-S3"

  expect_equal(foo() + foo_S3, "S7-S3")
  expect_equal(foo_S3 + foo(), "S3-S7")
})

test_that("Ops generics dispatch to S7 methods for S4 classes", {
  local_methods(base_ops[["+"]])
  fooS4 <- local_S4_class("foo", contains = "character")
  fooS7 <- new_class("foo")

  method(`+`, list(fooS7, fooS4)) <- function(e1, e2) "S7-S4"
  method(`+`, list(fooS4, fooS7)) <- function(e1, e2) "S4-S7"

  expect_equal(fooS4() + fooS7(), "S4-S7")
  expect_equal(fooS7() + fooS4(), "S7-S4")
})

test_that("Ops generics dispatch to S7 methods for POSIXct", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])
  foo <- new_class("foo")

  method(`+`, list(foo, class_POSIXct)) <- function(e1, e2) "foo-POSIXct"
  expect_equal(foo() + Sys.time(), "foo-POSIXct")

  method(`+`, list(class_POSIXct, foo)) <- function(e1, e2) "POSIXct-foo"
  expect_equal(Sys.time() + foo(), "POSIXct-foo")
})

test_that("Ops generics dispatch to S7 methods for NULL", {
  local_methods(base_ops[["+"]])
  foo <- new_class("foo")

  method(`+`, list(foo, NULL)) <- function(e1, e2) "foo-NULL"
  method(`+`, list(NULL, foo)) <- function(e1, e2) "NULL-foo"

  expect_equal(foo() + NULL, "foo-NULL")
  expect_equal(NULL + foo(), "NULL-foo")
})

test_that("`%*%` dispatches to S7 methods", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])

  ClassX <- new_class("ClassX")
  method(`%*%`, list(ClassX, class_any)) <- function(x, y) "ClassX %*% class_any"
  method(`%*%`, list(class_any, ClassX)) <- function(x, y) "class_any %*% ClassX"

  expect_equal(ClassX() %*% ClassX(), "ClassX %*% class_any")
  expect_equal(ClassX() %*% 1, "ClassX %*% class_any")
  expect_equal(1 %*% ClassX(), "class_any %*% ClassX")
})

