test_that("can provide Math group generic", {
  local_methods(S7_Math)
  foo1 <- new_class("foo1", properties = list(x = class_double, y = class_double))
  foo2 <- new_class("foo2", class_double)

  # base behaviour
  expect_snapshot(abs(foo1(-1, 2)), error = TRUE)
  expect_equal(abs(foo2(c(-1, 2))), foo2(c(1, 2)))

  method(S7_Math, foo1) <- function(x, ..., .Generic) {
    .Generic <- find_base_generic(.Generic)
    foo1(.Generic(x@x, ...), .Generic(x@y, ...))
  }
  expect_equal(abs(foo1(-1, 2)), foo1(1, 2))

  method(S7_Math, foo2) <- function(x, ..., .Generic) {
    .Generic <- find_base_generic(.Generic)
    foo2(.Generic(S7_data(x, ...)))
  }
  expect_equal(abs(foo2(c(-1, 2))), foo2(c(1, 2)))
})
