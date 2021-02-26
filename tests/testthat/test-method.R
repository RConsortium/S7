test_that("methods can be registered for a generic and then called", {
  foo <- generic_new(name = "foo", signature = alist(x=))
  method_register(foo, "text", function(x) paste0("foo-", x@.data))

  expect_equal(foo(text("bar")), "foo-bar")
})

test_that("single inheritance works when searching for methods", {
  foo2 <- generic_new(name = "foo2", signature = alist(x=))

  method_register(foo2, "character", function(x) paste0("foo2-", x))

  expect_equal(foo2(text("bar")), "foo2-bar")
})

test_that("direct multiple dispatch works", {
  foo3 <- generic_new(name = "foo3", signature = alist(x=, y=))
  method_register(foo3, list("text", "number"), function(x, y) paste0(x, y))
  expect_equal(foo3(text("bar"), number(1)), "bar1")
})

test_that("inherited multiple dispatch works", {
  foo4 <- generic_new(name = "foo4", signature = alist(x=, y=))
  method_register(foo4, list("character", "numeric"), function(x, y) paste0(x, ":", y))

  expect_equal(foo4(text("bar"), number(1)), "bar:1")
})

test_that("method dispatch works for S3 objects", {
  foo <- generic_new(name = "foo", signature = "x")

  obj <- structure("hi", class = "my_s3")

  method_register(foo, "my_s3", function(x) paste0("foo-", x))

  expect_equal(foo(obj), "foo-hi")
})

test_that("method dispatch works for S3 objects", {
  skip_if_not(requireNamespace("methods"))

  Range <- setClass("Range", slots = c(start = "numeric", end = "numeric"))
  obj <- Range(start = 1, end = 10)

  foo <- generic_new(name = "foo", signature = "x")

  method_register(foo, "Range", function(x) paste0("foo-", x@start, "-", x@end))

  expect_equal(foo(obj), "foo-1-10")
})

test_that("method_register works if you use r7 class objects", {
  foo5 <- generic_new(name = "foo5", signature = alist(x=, y=))
  method_register(foo5, list(text, number), function(x, y) paste0(x, ":", y))

  expect_equal(foo5(text("bar"), number(1)), "bar:1")
})

test_that("method_register works if you pass a bare class", {
  foo6 <- generic_new(name = "foo6", signature = alist(x=))
  method_register(foo6, text, function(x) paste0("foo-", x))

  expect_equal(foo6(text("bar")), "foo-bar")
})

test_that("method_register works if you pass a bare class union", {
  foo7 <- generic_new(name = "foo7", signature = alist(x=))
  method_register(foo7, class_union(text, number), function(x) paste0("foo-", x))

  expect_equal(foo7(text("bar")), "foo-bar")
  expect_equal(foo7(number(1)), "foo-1")
})
