describe("R7_class", {
  my_class <- new_class("my_class")
  it("has a character name", {
    expect_type(my_class@name, "character")
  })
  it("has a parent", {
    expect_s3_class(my_class@parent, "R7_class")
  })
  it("is an instance of R7_class and object", {
    expect_equal(class(my_class), c("R7_class", "R7_object"))
  })
  it("are first class R7 objects themselves", {
    expect_equal(object_class(my_class), my_class)
  })
  it("has a constructor function", {
    expect_type(my_class@constructor, "closure")
  })
  it("has a validator function", {
    expect_type(my_class@validator, "closure")
  })
  it("has a list of properties", {
    expect_type(my_class@properties, "list")
  })
  it("can be printed", {
    expect_snapshot(my_class)
  })
})

test_that("classes can inherit from base types", {
  types <- c("logical", "integer", "complex", "character", "raw", "list")
  for (type in types) {
    f <- match.fun(type)
    foo <- new_class("foo", parent = type, constructor = function(x = f()) new_object(x))
    obj <- foo()
    expect_equal(typeof(obj@.data), type)
  }

  foo <- new_class("foo", parent = "numeric", constructor = function(x = numeric()) new_object(x))
  obj <- foo()
  expect_equal(typeof(obj@.data), "double")

  foo <- new_class("foo", parent = "function", constructor = function(x = function() NULL, ...) new_object(x))
  obj <- foo()
  expect_equal(typeof(obj@.data), "closure")
})

test_that("classes can use unions in properties", {
  my_class <- new_class("my_class", properties = list(new_property(name = "name", new_union("character", "factor"))))

  expect_equal(my_class(name = "foo")@name, "foo")
  expect_equal(my_class(name = factor("foo"))@name, factor("foo"))

  expect_snapshot_error(my_class(name = 1))
})


# constructor -------------------------------------------------------------

test_that("generates correct arguments from parent + properties",  {
  # No arguments
  args <- constructor_args(R7_object)
  expect_equal(args$constructor, character())

  # Includes properties
  args <- constructor_args(R7_object, as_properties(list(x = "numeric")))
  expect_equal(args$constructor, "x")

  # unless they're dynamic
  args <- constructor_args(R7_object,
    as_properties(list(new_property("x", getter = function(x) 10)))
  )
  expect_equal(args$constructor, character())

  # Includes parent properties
  foo <- new_class("foo", properties = list(x = "numeric"))
  args <- constructor_args(foo, as_properties(list(y = "numeric")))
  expect_equal(args$constructor, c("x", "y"))
  expect_equal(args$self, "y")
  expect_equal(args$parent, "x")

  # But only those in the constructor
  foo <- new_class("foo",
    properties = list(x = "numeric"),
    constructor = function() new_object(x = 1)
  )
  args <- constructor_args(foo, as_properties(list(y = "numeric")))
  expect_equal(args$constructor, "y")
  expect_equal(args$self, "y")
  expect_equal(args$parent, character())
})

test_that("generates meaningful constructors", {
  expect_snapshot({
    foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"))
    foo@constructor

    foo <- new_class("foo", parent = "character")
    foo@constructor

    foo2 <- new_class("foo2", parent = foo)
    foo2@constructor
  }, transform = scrub_environment)
})
