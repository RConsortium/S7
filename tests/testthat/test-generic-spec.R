test_that("can standardise generics", {
  foo_R7 <- new_generic("foo", dispatch_args = "x")
  methods::setGeneric("foo_S4", function(x) {})

  expect_equal(as_generic(foo_R7), foo_R7)
  expect_equal(as_generic(foo_S4), foo_S4)

  expect_equal(as_generic(sum), S3_generic(sum, "sum"))
  expect_equal(as_generic(mean), S3_generic(mean, "mean"))

  expect_snapshot(as_generic(function() {}), error = TRUE)
  expect_snapshot(as_generic(1), error = TRUE)
})
