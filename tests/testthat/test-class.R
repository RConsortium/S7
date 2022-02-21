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

test_that("new_class() checks its inputs", {
  expect_snapshot(error = TRUE, {
    new_class(1)
    new_class("foo", 1)
  })
})

test_that("classes can inherit from base types", {
  for (class in base_classes) {
    foo <- new_class("foo", parent = class)
    expect_error(foo(), NA)
  }
})

test_that("classes can't inherit from S4 or class unions", {
  parentS4 <- methods::setClass("parentS4", slots = c(x = "numeric"))
  expect_snapshot(error = TRUE, {
    new_class("test", parent = parentS4)
    new_class("test", parent = new_union("character"))
  })
})

describe("default constructor", {
  it("initializes properties with defaults", {
    foo1 <- new_class("foo1", properties = list(x = "integer"))
    obj <- foo1()
    expect_equal(obj@x, integer())

    foo2 <- new_class("foo2", foo1, properties = list(y = "integer"))
    obj <- foo2()
    expect_equal(obj@x, integer())
    expect_equal(obj@y, integer())
  })

  it("initializes data with defaults", {
    text1 <- new_class("text1", parent = "character")
    obj <- text1()
    expect_equal(R7_data(obj), character())
  })

  it("initializes property with R7 object", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", properties = list(x = foo1))
    x <- foo2()
    expect_s3_class(x@x, "foo1")
  })
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
