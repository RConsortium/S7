test_that("s3_class has a print method", {
  expect_snapshot(s3_class(c("ordered", "factor")))
})

test_that("can construct objects that extend S3 classes", {
  foo <- s3_class("foo", function(.data) structure(list(), class = "foo"))
  foo2 <- new_class("foo2", foo)
  expect_s3_class(foo2(), "foo")
})

test_that("subclasses inherit validator", {
  foo <- s3_class("foo",
    function(.data) structure(.data, class = "foo"),
    function(x) if (!is.double(x)) "Underlying data must be a double"
  )
  foo2 <- new_class("foo2", foo)

  expect_snapshot(error = TRUE, foo2("a"))
})


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
