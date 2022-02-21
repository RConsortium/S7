test_that("new_S3_class has a print method", {
  expect_snapshot(new_S3_class(c("ordered", "factor")))
})

test_that("can construct objects that extend S3 classes", {
  ordered2 <- new_class("ordered2", parent = S3_factor)
  x <- ordered2(c(1L, 2L, 1L), letters[1:3])
  expect_equal(class(x), c("ordered2", "R7_object", "factor"))
  expect_equal(prop_names(x), character())
  expect_error(x@levels, "Can't find property")
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
