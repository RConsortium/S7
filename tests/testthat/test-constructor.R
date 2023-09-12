test_that("generates correct arguments from parent + properties",  {
  # No arguments
  args <- constructor_args(S7_object)
  expect_equal(args$self, character())
  expect_equal(args$parent, character())

  # Includes properties
  args <- constructor_args(S7_object, as_properties(list(x = class_numeric)))
  expect_equal(args$self, "x")
  expect_equal(args$parent, character())

  # unless they're dynamic
  args <- constructor_args(S7_object,
    as_properties(list(x = new_property(getter = function(self) 10)))
  )
  expect_equal(args$self, character())
  expect_equal(args$parent, character())

  # Includes parent properties
  foo <- new_class("foo", properties = list(x = class_numeric))
  args <- constructor_args(foo, as_properties(list(y = class_numeric)))
  expect_equal(args$self, "y")
  expect_equal(args$parent, "x")

  # But only those in the constructor
  foo <- new_class("foo",
    properties = list(x = class_numeric),
    constructor = function() new_object(x = 1)
  )
  args <- constructor_args(foo, as_properties(list(y = class_numeric)))
  expect_equal(args$self, "y")
  expect_equal(args$parent, character())
})

test_that("generates meaningful constructors", {
  expect_snapshot({
    new_constructor(S7_object, list())
    new_constructor(S7_object, as_properties(list(x = class_numeric, y = class_numeric)))

    foo <- new_class("foo", parent = class_character)
    new_constructor(foo, list())

    foo2 <- new_class("foo2", parent = foo)
    new_constructor(foo2, list())
  }, transform = scrub_environment)
})

test_that("can generate constructors for S3 classes", {
  expect_snapshot({
    new_constructor(class_factor, list())
    new_constructor(class_factor, as_properties(list(x = class_numeric, y = class_numeric)))
  }, transform = scrub_environment)
})

test_that("can generate constructor for inherited abstract classes", {
  expect_snapshot({
    foo1 <- new_class("foo1", abstract = TRUE, properties = list(x = class_double))
    new_constructor(foo1, list())
    new_constructor(foo1, as_properties(list(y = class_double)))
  }, transform = scrub_environment)
  child <- new_class("child", foo1, properties = list(y = class_double))
  expect_no_error(child(y = 0.5))

  # even if it has a read-only property
  prop_readonly <- new_property(getter = function(self) "test")
  child <- new_class("child", foo1, properties = list(x = prop_readonly))
  expect_no_error(child())
})

test_that("can use `...` in parent constructor", {
  foo <- new_class(
    "foo",
    properties = list(x = class_list),
    constructor = function(...) new_object(S7_object(), x = list(...))
  )

  expect_snapshot(
    new_constructor(foo, list(y = class_double)),
    transform = scrub_environment
  )

  # And check that arguments matched correctly
  bar <- new_class("bar", foo, properties = list(y = class_double))
  expect_equal(bar()@x, list())
  expect_equal(bar(2)@x, list(2))
  expect_equal(bar(y = 2)@x, list())
})
