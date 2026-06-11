test_that("can inherit from environments", {
  Foo := new_class(
    parent = class_environment,
    properties = list(name = class_character),
    package = NULL
  )
  e <- Foo(new.env(parent = emptyenv()), name = "bob")
  expect_true(S7_inherits(e, Foo))
  expect_true(is.environment(e))
  expect_equal(e@name, "bob")

  # Property setters and method dispatch work
  e@name <- "alice"
  expect_equal(e@name, "alice")

  gen := new_generic("x")
  method(gen, Foo) <- function(x) "foo"
  expect_equal(gen(e), "foo")

  # S7_data() / S7_data<- refuse to operate on environments because they
  # would destroy `e`'s S7 attributes in place.
  expect_snapshot(error = TRUE, {
    S7_data(e)
    S7_data(e) <- new.env()
  })
})

test_that("str() and print() work for environment-derived classes", {
  Foo := new_class(
    parent = class_environment,
    properties = list(name = class_character),
    package = NULL
  )
  e <- Foo(new.env(parent = emptyenv()), name = "bob")

  expect_snapshot(
    {
      str(e)
      print(e)
    },
    transform = scrub_environment
  )
})

test_that("can't upcast an environment", {
  Parent := new_class(class_environment, package = NULL)
  Child := new_class(Parent, package = NULL)

  expect_snapshot(convert(Child(), Parent), error = TRUE)
})
