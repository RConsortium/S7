test_that("displays nicely", {
  foo <- new_external_class("package", "name", function() new_class("foo"))
  expect_snapshot(print(foo))
})
