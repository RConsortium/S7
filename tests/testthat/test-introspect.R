test_that("S7_classes() / S7_generics() inspect a single environment", {
  # Namespace: restricted to exports
  expect_equal(S7_classes(asNamespace("S7")), "S7_object")
  expect_equal(S7_generics(asNamespace("S7")), "convert")
})

test_that("default `env` is the caller's environment", {
  local({
    Foo <- new_class("Foo", package = NULL)
    Bar <- new_class("Bar", package = NULL)
    my_gen <- new_generic("my_gen", "x")

    expect_setequal(S7_classes(), c("Foo", "Bar"))
    expect_setequal(S7_generics(), "my_gen")
  })

  expect_setequal(S7_classes(), character())
  expect_setequal(S7_generics(), character())
})

test_that("find_objects() returns matching names", {
  env <- new.env(parent = emptyenv())
  env$Foo <- new_class("Foo", package = NULL)
  env$bar <- 1L
  env$Baz <- new_class("Baz", package = NULL)

  expect_setequal(find_objects(env, is_class), c("Foo", "Baz"))
  expect_setequal(find_objects(env, is.integer), "bar")
})
