test_that("can test and print", {
  expect_true(is_missing_class(missing_class))
  expect_false(is_missing_class(any_class))
  expect_true(is_any_class(any_class))
  expect_false(is_any_class(missing_class))

  expect_snapshot({
    print(missing_class)
    print(any_class)

    str(list(m = missing_class, a = any_class))
  })
})
