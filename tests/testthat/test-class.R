describe("R7_class", {
  my_class <- new_class("my_class")
  it("has a character name", {
    expect_type(my_class@name, "character")
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
})

test_that("classes can inherit from base types", {
  types <- c("logical", "integer", "double", "complex", "character", "raw", "list", "closure")
  for (type in types) {
    foo <- new_class("foo", parent = type)
    obj <- foo()
    expect_equal(typeof(obj@.data), type)
  }

  foo <- new_class("foo", parent = "numeric")
  obj <- foo()
  expect_equal(typeof(obj@.data), "double")

  foo <- new_class("foo", parent = "function")
  obj <- foo()
  expect_equal(typeof(obj@.data), "closure")
})
