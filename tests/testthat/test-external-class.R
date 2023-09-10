test_that("displays nicely", {
  foo <- new_external_class("package", "name", function() NULL)
  expect_snapshot(print(foo))
})
