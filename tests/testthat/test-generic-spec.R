test_that("can standardise generics", {
  foo_S7 <- new_generic("foo", "x")
  methods::setGeneric("foo_S4", function(x) {})

  expect_equal(as_generic(foo_S7), foo_S7)
  expect_equal(as_generic(foo_S4), foo_S4)

  expect_equal(as_generic(sum), S3_generic(sum, "sum"))
  expect_equal(as_generic(mean), S3_generic(mean, "mean"))

  expect_snapshot(as_generic(function() {}), error = TRUE)
  expect_snapshot(as_generic(1), error = TRUE)
})

test_that("base ops use S7 shim", {
  expect_equal(as_generic(`+`), base_ops[["+"]])
  if(getRversion() >= "4.3.0")
    expect_equal(as_generic(`%*%`), base_ops[["%*%"]])
})
