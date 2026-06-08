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

  expect_error(
    foo1() + new_class("foo3")(),
    class = "S7_error_method_not_found"
  )
})

test_that("Ops generics dispatch to S3 methods", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])
  defer(unregister_s3_methods(baseenv(), "Ops"))

  foo <- new_class("foo")
  method(`+`, list(class_factor, foo)) <- function(e1, e2) "factor-foo"
  method(`+`, list(foo, class_factor)) <- function(e1, e2) "foo-factor"

  expect_equal(foo() + factor(), "foo-factor")
  expect_equal(factor() + foo(), "factor-foo")

  # Even if custom method exists
  foo_S3 <- structure(list(), class = "foo_S3")
  local_s3_method("+.foo_S3", function(e1, e2) stop("Failure!"))

  method(`+`, list(new_S3_class("foo_S3"), foo)) <- function(e1, e2) "S3-S7"
  method(`+`, list(foo, new_S3_class("foo_S3"))) <- function(e1, e2) "S7-S3"

  expect_equal(foo() + foo_S3, "S7-S3")
  expect_equal(foo_S3 + foo(), "S3-S7")
})

test_that("operator methods on S3/S4 classes work when neither operand is S7", {
  local_methods(base_ops[["+"]], base_ops[["*"]])

  class_foo <- new_S3_class("foo")
  foo <- structure(list(), class = "foo")
  method(`+`, list(class_foo, class_any)) <- function(e1, e2) "foo+any"

  # Primitive `+` dispatches into S7 even though `foo` is not an S7 object
  expect_equal(foo + 10, "foo+any")
  # An unregistered operator falls back to the base behaviour
  expect_error(foo * 10, regexp = "non-numeric argument")

  fooS4 <- local_S4_class("fooS4")
  method(`+`, list(fooS4, class_any)) <- function(e1, e2) "fooS4+any"
  expect_equal(fooS4("x") + 10, "fooS4+any")
})

test_that("operator bridge does not clobber an existing group method", {
  local_methods(base_ops[["+"]])
  defer(unregister_s3_methods(baseenv(), "Ops"))

  local_s3_method("Ops.myS3", function(e1, e2) "myS3-ops")
  method(`+`, list(new_S3_class("myS3"), class_any)) <- function(e1, e2) "!"

  # + method used for S7 classes
  x <- structure(list(), class = "myS3")
  foo <- new_class("foo")
  expect_equal(x + foo(), "!")

  # but `Ops.myS3` used for base/S3 classes
  expect_equal(x + 10, "myS3-ops")
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
  # In R's C sources DispatchGroup() has special cases for POSIXt/Date/difftime
  # so we need to double check that S7 methods still take precedence:
  # https://github.com/wch/r-source/blob/5cc4e46fc/src/main/eval.c#L4242C1-L4247C64

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

test_that("Ops generics falls back to base behaviour", {
  local_methods(base_ops[["+"]])

  foo <- new_class("foo", parent = class_double)
  expect_equal(foo(1) + 1, foo(2))
  expect_equal(foo(1) + 1:2, 2:3)
  expect_equal(1 + foo(1), foo(2))
  expect_equal(1:2 + foo(1), 2:3)

  # but can be overridden
  method(`+`, list(foo, class_numeric)) <- function(e1, e2) "foo-numeric"
  method(`+`, list(class_numeric, foo)) <- function(e1, e2) "numeric-foo"
  expect_equal(foo(1) + 1, "foo-numeric")
  expect_equal(foo(1) + 1:2, "foo-numeric")
  expect_equal(1 + foo(1), "numeric-foo")
  expect_equal(1:2 + foo(1), "numeric-foo")
})

test_that("`%*%` dispatches to S7 methods", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])

  ClassX <- new_class("ClassX")
  method(`%*%`, list(ClassX, class_any)) <- function(x, y) {
    "ClassX %*% class_any"
  }
  method(`%*%`, list(class_any, ClassX)) <- function(x, y) {
    "class_any %*% ClassX"
  }

  expect_equal(ClassX() %*% ClassX(), "ClassX %*% class_any")
  expect_equal(ClassX() %*% 1, "ClassX %*% class_any")
  expect_equal(1 %*% ClassX(), "class_any %*% ClassX")
})

test_that("Ops methods can use super", {
  foo <- new_class("foo", class_integer)
  foo2 <- new_class("foo2", foo)

  method(`+`, list(foo, class_double)) <- function(e1, e2) {
    foo(S7_data(e1) + as.integer(e2))
  }
  method(`+`, list(foo2, class_double)) <- function(e1, e2) {
    foo2(super(e1, foo) + e2)
  }

  expect_equal(foo2(1L) + 1, foo2(2L))
})
