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
  foo <- new_class("foo", class_list)
  x <- foo()
  x$x <- 1
  expect_equal(x$x, 1)
})

test_that("[ gives more accurate error", {
  expect_snapshot(error = TRUE, {
    x <- new_class("foo")()
    x[1]
    x[1] <- 1
  })

  # but ok if inheriting from list
  x <- new_class("foo", class_list)()
  x[1] <- 1
  expect_equal(x[1], list(1))
})

test_that("[[ gives more accurate error", {
  expect_snapshot(error = TRUE, {
    x <- new_class("foo")()
    x[[1]]
    x[[1]] <- 1
  })

  # but ok if inheriting from list
  x <- new_class("foo", class_list)()
  x[[1]] <- 1
  expect_equal(x[[1]], 1)

  # and ok if inheriting from environment
  x <- new_class("foo", class_environment)()
  x[["a"]] <- 1
  expect_equal(x[["a"]], 1)
})

test_that("register S4 classes for key components", {
  expect_s4_class(getClass("R7_object"), "classRepresentation")
  expect_s4_class(getClass("R7_method"), "classRepresentation")
  expect_s4_class(getClass("R7_generic"), "classRepresentation")
})
