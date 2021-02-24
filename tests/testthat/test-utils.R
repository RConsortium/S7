test_that("is_named works", {
  # No names
  expect_false(is_named(c("x", "y")))

  # Only partial names
  expect_false(is_named(c(x = "x", "y")))

  # All names
  expect_true(is_named(c(x = "x",y = "y")))
})

test_that("normalize_signature errors appropriately", {
  expect_error(normalize_signature(text), "must either be named types")

  expect_error(normalize_signature(list(text)), "must either be named types")
})

test_that("normalize_signature works with unnamed character vectors", {
  expect_equal(normalize_signature(c("x", "y")), alist(x=, y=))
})

test_that("normalize_signature works with named character vectors", {
  expect_equal(normalize_signature(c("x" = "character", "y" = "numeric")), list(x=class_get("character"), y=class_get("numeric")))
})
