test_that("S7_class validates its underlying data", {
  x <- new_class("X", package = NULL)()
  expect_snapshot_error(S7_data(x) <- 1)
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
})

test_that("register S4 classes for key components", {
  expect_s4_class(getClass("S7_object"), "classRepresentation")
  expect_s4_class(getClass("S7_method"), "classRepresentation")
  expect_s4_class(getClass("S7_generic"), "classRepresentation")
})

test_that("S7 methods can be traced", {
  my_generic <- new_generic("my_generic", "x")
  my_class <- new_class("my_class", package = NULL)
  method(my_generic, my_class) <- function(x) "result"
  original <- method(my_generic, my_class)
  obj <- my_class()

  calls <- new.env()
  calls$n <- 0
  tracer <- function() calls$n <- calls$n + 1

  suppressMessages(
    trace("my_class", tracer, print = FALSE, where = my_generic@methods)
  )
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
  expect_output(
    print(method(my_generic, my_class)),
    "<S7_method>",
    fixed = TRUE
  )

  suppressMessages(untrace("my_class", where = my_generic@methods))
  expect_identical(method(my_generic, my_class), original)
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
})

test_that("S7 generics can be traced", {
  my_generic <- new_generic("my_generic", "x")
  my_class <- new_class("my_class", package = NULL)
  method(my_generic, my_class) <- function(x) "result"
  obj <- my_class()

  calls <- new.env()
  calls$n <- 0
  tracer <- function() calls$n <- calls$n + 1

  suppressMessages(
    trace("my_generic", tracer, print = FALSE, where = environment())
  )
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
  expect_output(print(my_generic), "<S7_generic>", fixed = TRUE)

  suppressMessages(untrace("my_generic", where = environment()))
  expect_s3_class(my_generic, "S7_generic")
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
})
