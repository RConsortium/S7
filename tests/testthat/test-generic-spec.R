test_that("can standardise generics", {
  foo_S7 <- new_generic("foo", "x")
  methods::setGeneric("foo_S4", function(x) {})

  expect_equal(as_generic(foo_S7), foo_S7)
  expect_equal(as_generic(foo_S4), foo_S4)

  expect_equal(as_generic(sum), S3_generic(sum, "sum", "base"))
  expect_equal(as_generic(mean), S3_generic(mean, "mean", "base"))

  expect_snapshot(as_generic(function() {}), error = TRUE)
  expect_snapshot(as_generic(1), error = TRUE)
})

test_that("can find package associated with S3 generic", {
  expect_equal(find_package(mean, "mean"), "base")
  expect_equal(find_package(median, "median"), "stats")
  expect_equal(find_package(utils::as.person, "as.person"), "utils")
})

test_that("clear error if can't find generic", {
  expect_snapshot(find_package(tibble::as_tibble, "as_tibble"), error = TRUE)
})

test_that("base ops use S7 shim", {
  expect_equal(as_generic(`+`), base_ops[["+"]])
  if(getRversion() >= "4.3.0")
    expect_equal(as_generic(`%*%`), base_ops[["%*%"]])
})
