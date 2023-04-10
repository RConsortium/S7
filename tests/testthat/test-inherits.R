test_that("it works", {
  foo1 <- new_class("foo1")
  foo2 <- new_class("foo2", parent = foo1)

  expect_true(S7_inherits(foo1(), foo1))
  expect_true(S7_inherits(foo2(), foo1))
  expect_false(S7_inherits(foo1(), foo2))
})

test_that("checks that input is a class", {
  expect_snapshot(S7_inherits(1:10, "x"), error = TRUE)
})

test_that("throws informative error", {
  expect_snapshot(error = TRUE, {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2")
    check_is_S7(foo1(), foo2)
  })
  expect_snapshot(check_is_S7("a"), error = TRUE)
})
