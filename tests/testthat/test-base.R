test_that("validation gives useful message", {
  expect_equal(base_classes$integer$validator(1L), NULL)
  expect_snapshot(base_classes$integer$validator(TRUE))
})
