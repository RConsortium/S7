test_that("generates correct arguments from parent + properties",  {
  # No arguments
  args <- constructor_args(R7_object)
  expect_equal(args$self, character())
  expect_equal(args$parent, character())

  # Includes properties
  args <- constructor_args(R7_object, as_properties(list(x = "numeric")))
  expect_equal(args$self, "x")
  expect_equal(args$parent, character())

  # unless they're dynamic
  args <- constructor_args(R7_object,
    as_properties(list(x = new_property(getter = function(self) 10)))
  )
  expect_equal(args$self, character())
  expect_equal(args$parent, character())

  # Includes parent properties
  foo <- new_class("foo", properties = list(x = "numeric"))
  args <- constructor_args(foo, as_properties(list(y = "numeric")))
  expect_equal(args$self, "y")
  expect_equal(args$parent, "x")

  # But only those in the constructor
  foo <- new_class("foo",
    properties = list(x = "numeric"),
    constructor = function() new_object(x = 1)
  )
  args <- constructor_args(foo, as_properties(list(y = "numeric")))
  expect_equal(args$self, "y")
  expect_equal(args$parent, character())
})

test_that("generates meaningful constructors", {
  expect_snapshot({
    new_constructor(R7_object, list())
    new_constructor(R7_object, as_properties(list(x = "numeric", y = "numeric")))

    foo <- new_class("foo", parent = "character")
    new_constructor(foo, list())

    foo2 <- new_class("foo2", parent = foo)
    new_constructor(foo2, list())
  }, transform = scrub_environment)
})

test_that("can generate constructors for S3 classes", {
  expect_snapshot({
    new_constructor(S3_factor, list())
    new_constructor(S3_factor, as_properties(list(x = "numeric", y = "numeric")))
  }, transform = scrub_environment)
})
