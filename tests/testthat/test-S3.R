
test_that("s3_class() checks its inputs", {
  expect_snapshot(s3_class(1), error = TRUE)

  expect_snapshot(error = TRUE, {
    s3_class("foo", function(x) {})
    s3_class("foo", function(.data, ...) {})
  })
})

test_that("default s3_class constructor errors", {
  # constructor errors if needed
  expect_snapshot(class_construct(s3_class("foo")), error = TRUE)
})
