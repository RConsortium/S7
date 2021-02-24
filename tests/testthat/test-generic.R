test_that("normalize_signature errors appropriately", {
  expect_error(normalize_signature(text), "must either be named types")

  expect_error(normalize_signature(list(text)), "must either be named types")
})

test_that("normalize_signature works with unnamed character vectors", {
  expect_equal(normalize_signature(c("x", "y")), alist(x=, y=))
})
