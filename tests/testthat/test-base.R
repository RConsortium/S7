test_that("validation uses typeof", {
  expect_equal(class_integer$validator(1L), NULL)
  expect_equal(class_integer$validator(factor()), NULL)
  expect_snapshot(class_integer$validator(TRUE))

  expect_equal(class_function$validator(`[`), NULL)
  expect_equal(class_function$validator(sum), NULL)
  expect_equal(class_function$validator(mean), NULL)
})

test_that("base class display as expected", {
  expect_snapshot({
    class_integer
    str(class_integer)
  })
})

test_that("classes can inherit from base types", {
  base_classes <- c(class_vector$classes, list(class_function))

  for (class in base_classes) {
    foo <- new_class("foo", parent = class)
    expect_error(foo(), NA)
  }
})
