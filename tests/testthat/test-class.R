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
})

test_that("classes can inherit from base types", {
  types <- list(logical = lgl, integer = int, complex = cpl, character = chr, raw = raw, list = lst)
  for (i in seq_along(types)) {
    f <- match.fun(names(types)[[i]])
    type <- types[[i]]
    foo <- new_class("foo", parent = type, constructor = function(x = f()) new_object(x))
    obj <- foo()
    expect_equal(typeof(obj@.data), names(types)[[i]])
  }

  foo <- new_class("foo", parent = num, constructor = function(x = numeric()) new_object(x))
  obj <- foo()
  expect_equal(typeof(obj@.data), "double")

  foo <- new_class("foo", parent = fn, constructor = function(x = function() NULL, ...) new_object(x))
  obj <- foo()
  expect_equal(typeof(obj@.data), "closure")
})

test_that("classes can use unions in properties", {
  my_class <- new_class("my_class", properties = list(new_property(name = "name", new_union(chr, fct))))

  expect_equal(my_class(name = "foo")@name, "foo")
  expect_equal(my_class(name = factor("foo"))@name, factor("foo"))

  expect_snapshot_error(my_class(name = 1))
})
