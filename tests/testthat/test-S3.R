test_that("new_S3_class has a print method", {
  expect_snapshot(new_S3_class(c("ordered", "factor")))
})

test_that("can construct objects that extend S3 classes", {
  ordered2 <- new_class("ordered2", parent = class_factor, package = NULL)
  x <- ordered2(c(1L, 2L, 1L), letters[1:3])
  expect_equal(class(x), c("ordered2", "factor", "S7_object"))
  expect_equal(prop_names(x), character())
  expect_error(x@levels, "Can't find property")
})

test_that("subclasses inherit validator", {
  foo <- new_S3_class("foo",
    function(.data) structure(.data, class = "foo"),
    function(x) if (!is.double(x)) "Underlying data must be a double"
  )
  foo2 <- new_class("foo2", foo, package = NULL)

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

test_that("can construct data frame subclass", {
  dataframe2 <- new_class("dataframe2", class_data.frame)
  df <- dataframe2(list(x = 1:3))
  expect_s3_class(df, "data.frame")
})

# Basic tests of validators -----------------------------------------------

test_that("catches invalid factors", {
  expect_snapshot({
    validate_factor(structure("x"))
  })
})

test_that("catches invalid dates", {
  expect_snapshot({
    validate_date("x")
  })
})

test_that("catches invalid POSIXct", {
  expect_snapshot({
    validate_POSIXct(structure("x", tz = "UTC"))
    validate_POSIXct(structure(1, tz = 1))
  })
})

test_that("catches invalid data.frame", {
  expect_snapshot({
    validate_data.frame(1)
    validate_data.frame(structure(list(x = 1, y = 1:2), row.names = 1L))
    validate_data.frame(structure(list(x = 1, y = 1), row.names = 1:2))
    validate_data.frame(structure(list(1), row.names = 1L))
  })
})
