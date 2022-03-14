test_that("R7_class validates its underlying data", {
  x <- new_class("X")()
  expect_snapshot_error(R7_data(x) <- 1)
})

test_that("$ gives useful error", {
  foo <- new_class("foo")
  x <- foo()
  expect_snapshot(error = TRUE, {
    x$y
    x$y <- 1
  })

  # But works as expected if inheriting from list
  foo <- new_class("foo", list)
  x <- foo()
  x$x <- 1
  expect_equal(x$x, 1)
})

test_that("register S4 classes for key components", {
  expect_s4_class(getClass("R7_object"), "classRepresentation")
  expect_s4_class(getClass("R7_method"), "classRepresentation")
  expect_s4_class(getClass("R7_generic"), "classRepresentation")
})
