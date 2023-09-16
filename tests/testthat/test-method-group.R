test_that("specific method overrides group generic", {
  foo <- new_class("foo", class_integer)

  method(`+`, list(foo, foo)) <- function(e1, e2) {
    foo(S7_data(e1) + S7_data(e2) + 100L)
  }
  method(group_generic_Ops, list(foo, foo)) <- function(e1, e2, .Generic) {
    foo(.Generic(S7_data(e1), S7_data(e2)))
  }

  expect_equal(foo(1L) * foo(1:5), foo(1:5))
  expect_equal(foo(1L) + foo(1:5), foo(1:5 + 101L))

})
