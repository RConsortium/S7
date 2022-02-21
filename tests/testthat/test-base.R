test_that("validation gives useful message", {
  expect_equal(base_classes$integer$validator(1L), NULL)
  expect_snapshot(base_classes$integer$validator(TRUE))
})

test_that("base class display as expected", {
  expect_snapshot({
    base_classes
    str(base_classes)
  })
})

test_that("classes can inherit from base types", {
  for (class in base_classes) {
    foo <- new_class("foo", parent = class)
    expect_error(foo(), NA)
  }
})
