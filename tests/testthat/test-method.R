test_that("methods can be registered for a generic and then called", {
  text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))
  foo <- generic_new(name = "foo", signature = alist(x=))
  method_register(foo, "text", function(x) paste0("foo-", x@.data))

  expect_equal(foo(text("bar")), "foo-bar")
})

test_that("single inheritance works when searching for methods", {
  text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))
  foo2 <- generic_new(name = "foo2", signature = alist(x=))

  method_register(foo2, "character", function(x) paste0("foo2-", x))

  expect_equal(foo2(text("bar")), "foo2-bar")
})

test_that("direct multiple dispatch works", {
  text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))
  number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

  foo3 <- generic_new(name = "foo3", signature = alist(x=, y=))
  method_register(foo3, list("text", "number"), function(x, y) paste0(x, y))
  expect_equal(foo3(text("bar"), number(1)), "bar1")
})

test_that("inherited multiple dispatch works", {
  text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))
  number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

  foo4 <- generic_new(name = "foo4", signature = alist(x=, y=))
  method_register(foo4, list("character", "numeric"), function(x, y) paste0(x, ":", y))

  expect_equal(foo4(text("bar"), number(1)), "bar:1")
})
