test_that("R7_class validates its underlying data", {
  x <- new_class("X")()
  expect_snapshot_error(R7_data(x) <- 1)
})
