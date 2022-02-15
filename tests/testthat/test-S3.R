test_that("new_S3_class has a print method", {
  expect_snapshot(new_S3_class("ordered"))
})

test_that("can construct objects that extend S3 classes", {
  foo <- new_S3_class("foo", function(.data) structure(list(), class = "foo"))
  foo2 <- new_class("foo2", foo)
  expect_s3_class(foo2(), "foo")
})

test_that("subclasses inherit validator", {
  foo <- new_S3_class("foo",
    function(.data) structure(.data, class = "foo"),
    function(x) if (!is.double(x)) "Underlying data must be a double"
  )
  foo2 <- new_class("foo2", foo)

  expect_snapshot(error = TRUE, foo2("a"))
})


test_that("new_S3_class() checks its inputs", {
  expect_snapshot(new_S3_class(1), error = TRUE)

  expect_snapshot(error = TRUE, {
    new_S3_class("foo", function(x) {})
    new_S3_class("foo", function(.data, ...) {})
  })
})


test_that("default new_S3_class constructor errors", {
  # constructor errors if needed
  expect_snapshot(class_construct(new_S3_class("foo"), 1), error = TRUE)
})
