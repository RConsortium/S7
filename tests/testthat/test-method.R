test_that("methods can be registered for a generic and then called", {
  text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
  foo <- r7_generic(name = "foo", signature = alist(x=))
  method_register("foo", "text", function(x) x@.data)

  expect_equal(foo(text("bar")), "bar")
})
