test_that("can test and print", {
  expect_true(is_class_missing(class_missing))
  expect_false(is_class_missing(class_any))
  expect_true(is_class_any(class_any))
  expect_false(is_class_any(class_missing))

  expect_snapshot({
    print(class_missing)
    print(class_any)

    str(list(m = class_missing, a = class_any))
  })
})

test_that("can union with |", {
  expect_equal(class_missing | NULL, new_union(class_missing, NULL))
  expect_equal(class_any | NULL, new_union(class_any, NULL))
})
