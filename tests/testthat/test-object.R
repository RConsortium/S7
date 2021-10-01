test_that("normal R7 objects have a class object attribute that is retrieved with `object_class()`", {
    x <- range(1, 10)
    obj_cls <- attr(x, "object_class")
    expect_equal(object_class(x), obj_cls)
})

test_that("normal R7 objects also for S3 compatibility, a class attribute, a character vector of class names", {
    x <- range(1, 10)
    cls <- attr(x, "class")
    expect_equal(class(x), cls)
    expect_equal(cls, c("range", "R7_object"))
})

test_that("Additional attributes storing properties defined by the class, accessible with `@/property()`", {
    x <- range(start = 1, end = 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
})

describe("new_object", {
  it("checks new objects for validity", {
    expect_error(range(start = 10, end = 1), "`end` must be greater than or equal to `start`")
  })


  it("can instantiate a new object that inherits from a basic type", {
    x <- text()
    expect_equal(x@.data, chr())

    y <- text("foo")
    expect_equal(y@.data, chr("foo"))
  })

  it("errors if given an invalid property", {
    expect_error(
      range(1, "foo"),
      "must be of class"
    )
  })

  it("can use the parent constructor to instantiate objects", {
    text2 <- new_class("text2", parent = chr)
    my_class <- new_class("my_class", parent = text2, properties = c(name = chr), constructor = function(x, name) new_object(text2(x), name = name))

    obj <- my_class("foo", "bar")

    expect_equal(obj@.data, text2("foo"))
  })
})

test_that("printing R7 objects work", {
  x <- range(1, 10)

  expect_snapshot(print(x))
})

test_that("printing R7 classes work", {
  skip("TODO")
  expect_snapshot(range)
})

test_that("object_class returns itself for R7_class objects", {
  text <- new_class("text", parent = chr)

  expect_equal(object_class(text), text)
})

test_that("object_class returns the object class property for R7_object objects", {
  text <- new_class("text", parent = chr)

  obj <- text("hi")

  expect_equal(object_class(obj), obj@object_class)
})

test_that("object_class returns class for basic types", {
  expect_equal(object_class("foo"), "character")
})

test_that("object_class returns class for S3 types", {
  foo <- structure(list(), class = "foo")
  expect_equal(object_class(foo), "foo")
})

test_that("object_class returns the class for S4 types", {
  foo <- methods::setClass("foo", representation = "character")
  obj <- foo("hi")
  expect_equal(object_class(obj), methods::extends(class(obj)))
})
