test_that("is_named works", {
  # No names
  expect_false(is_named(c("x", "y")))

  # Only partial names
  expect_false(is_named(c(x = "x", "y")))

  # All names
  expect_true(is_named(c(x = "x",y = "y")))
})

