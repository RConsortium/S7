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
  it("str yields all details when used at top-level", {
    expect_snapshot({
      str(my_class)
      str(range)
    })
  })
  it("str() summarises when nested", {
    expect_snapshot(list(range))
  })
})

test_that("classes can inherit from base types", {
  types <- c("logical", "integer", "double", "complex", "character", "raw", "list")
  for (type in types) {
    f <- match.fun(type)
    foo <- new_class("foo", parent = type, constructor = function(x = f()) new_object(x))
    obj <- foo()
    expect_equal(typeof(r7_data(obj)), type)
  }

  foo <- new_class("foo", parent = "function", constructor = function(x = function() NULL, ...) new_object(x))
  obj <- foo()
  expect_equal(typeof(r7_data(obj)), "closure")
})

test_that("classes can't inherit from S4 or class unions", {
  parentS4 <- methods::setClass("parentS4", slots = c(x = "numeric"))
  expect_snapshot(error = TRUE, {
    new_class("test", parent = parentS4)
    new_class("test", parent = new_union("character"))
  })
})

test_that("can supply literal examples of base types", {
  foo <- new_class("foo", parent = integer)
  obj <- foo(1L)
  expect_s3_class(obj, "integer")
  expect_type(r7_data(obj), "integer")
})

test_that("default constructor works", {
  foo1 <- new_class("foo1", properties = list(x = "numeric"))
  foo2 <- new_class("foo2", parent = foo1, properties = list(y = "numeric"))
  expect_s3_class(foo1(x = 1), "foo1")
  expect_s3_class(foo2(x = 1, y = 2), "foo2")

  text1 <- new_class("text1", parent = "character")
  text2 <- new_class("text2", parent = text1, properties = list(y = "numeric"))
  expect_s3_class(text1("abc"), "text1")
  expect_s3_class(text2("abc", y = 1), "text2")
})

test_that("constructor types check their values", {
  expect_snapshot_error(new_class("foo", parent = integer)("abc"))
})
