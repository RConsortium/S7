describe("new_object", {
  it("can instantiate a new object with properties", {
    x <- range(start = 1, end = 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
  })

  it("checks new objects for validity", {
    expect_error(range(start = 10, end = 1), "`end` must be greater than or equal to `start`")
  })


  it("can instantiate a new object that inherits from a basic type", {
    x <- text()
    expect_equal(x@.data, character())

    y <- text("foo")
    expect_equal(y@.data, "foo")
  })

  it("errors if given an invalid property", {
    expect_error(
      range(1, "foo"),
      "must be of class"
    )
  })
})

test_that("printing R7 objects work", {
  x <- range(1, 10)

  expect_snapshot(print(x))
})

test_that("printing R7 classes work", {
  expect_snapshot(range)
})

test_that("object_class returns itself for R7_class objects", {
  text <- new_class("text", parent = "character")

  expect_equal(object_class(text), text)
})

test_that("object_class returns the object class property for R7_object objects", {
  text <- new_class("text", parent = "character")

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
