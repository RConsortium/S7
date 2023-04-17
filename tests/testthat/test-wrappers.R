test_that("S7 objects error when used with common generics", {
  expect_snapshot(error = TRUE, {
    foo <- new_class("foo")
    mean(foo())
    sum(foo())
    sin(foo())
    Re(foo())
  })
})
