test_that("normalize_signature errors appropriately", {
  expect_error(normalize_signature(text), "must either be named types")

  expect_error(normalize_signature(list(text)), "must either be named types")
})

test_that("normalize_signature works with unnamed character vectors", {
  expect_equal(normalize_signature(c("x", "y")), alist(x=, y=, ...=))
})

test_that("generics pass ... to methods, and methods can define additional arguments", {
  foo <- new_generic(name = "foo", signature = "x")
  new_method(foo, "character", function(x, sep = "-") paste0("foo", sep, x))

  expect_equal(foo("bar"), "foo-bar")
  expect_equal(foo("bar", sep = "/"), "foo/bar")
})
