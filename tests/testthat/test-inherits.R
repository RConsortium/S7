test_that("it works", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", parent = foo1)

  expect_true(S7_inherits(foo1(), NULL))
  expect_true(S7_inherits(foo1(), foo1))
  expect_true(S7_inherits(foo2(), foo1))
  expect_false(S7_inherits(foo1(), foo2))
  expect_false(S7_inherits(1, NULL))
})

test_that("accepts any class specification (#556)", {
  # base
  expect_true(S7_inherits(1L, class_integer))
  expect_false(S7_inherits(1L, class_character))

  # S3
  expect_true(S7_inherits(factor("a"), new_S3_class("factor")))
  expect_false(S7_inherits(1L, new_S3_class("factor")))

  # union
  expect_true(S7_inherits(1L, class_integer | class_character))
  expect_false(S7_inherits(1.5, class_integer | class_character))

  # class_any
  expect_true(S7_inherits("anything", class_any))
})

test_that("checks that input is a class", {
  expect_snapshot(S7_inherits(1:10, "x"), error = TRUE)
})

test_that("throws informative error", {
  expect_snapshot(error = TRUE, {
    foo1 <- new_class("foo1", package = NULL)
    foo2 <- new_class("foo2", package = NULL)
    check_is_S7(foo1(), foo2)
  })
  expect_snapshot(check_is_S7("a"), error = TRUE)
})

test_that("check_is_S7() accepts any class specification (#556)", {
  expect_invisible(check_is_S7(1L, class_integer))
  expect_invisible(check_is_S7(factor("a"), new_S3_class("factor")))
  expect_invisible(check_is_S7(1L, class_integer | class_character))
  expect_invisible(check_is_S7("anything", class_any))

  expect_snapshot(error = TRUE, {
    check_is_S7(1L, class_character)
    check_is_S7(1.5, class_integer | class_character)
  })
})
