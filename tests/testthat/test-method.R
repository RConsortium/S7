test_that("methods can be registered for a generic and then called", {
  text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
  foo <- r7_generic(name = "foo", signature = alist(x=))
  method_register("foo", "text", function(x) paste0("foo-", x@.data))

  expect_equal(foo(text("bar")), "foo-bar")
})

test_that("single inheritance works when searching for methods", {
  text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
  foo2 <- r7_generic(name = "foo2", signature = alist(x=))

  method_register("foo2", "character", function(x) paste0("foo2-", x))

  expect_equal(foo2(text("bar")), "foo2-bar")
})
