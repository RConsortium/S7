test_that("method introspection can dispatch by class or object", {
  foo := new_generic("x")
  method(foo, class_character) <- function(x) "c"

  expect_equal(
    method(foo, class = class_character),
    method(foo, object = "x")
  )
})

test_that("method introspection errors on invalid inputs", {
  expect_snapshot(error = TRUE, {
    method(print, 1)

    foo := new_generic("x")
    method(foo)
    method(foo, 1)
    method(foo, new_union(class_integer, class_double))

    foo2 := new_generic(c("x", "y"))
    method(foo2, object = list(class_character))
  })
})

test_that("method introspection errors if no method found", {
  foo := new_generic("x")
  foo2 := new_generic(c("x", "y"))

  expect_snapshot(error = TRUE, {
    method(foo, class = class_integer)
    method(foo, object = 1L)

    method(foo2, class = list(class_integer, class_double))
    method(foo2, object = list(1L, 2))
  })
})

test_that("method explanation shows all possible methods along with matches", {
  foo1 := new_class(package = NULL)
  foo2 := new_class(foo1, package = NULL)

  add := new_generic(c("x", "y"))
  method(add, list(foo2, foo1)) <- function(x, y) c(2, 1)
  method(add, list(foo1, foo1)) <- function(x, y) c(1, 1)

  expect_snapshot_output(method_explain(add, list(foo2, foo2)))
})
