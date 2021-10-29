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
    expect_equal(x@.data, class_get("character")())

    y <- text("foo")
    expect_equal(y@.data, class_get("character")("foo"))
  })

  it("errors if given an invalid property", {
    expect_error(
      range(1, "foo"),
      "must be of class"
    )
  })

  it("checks are arguments are properties", {
    expect_snapshot(error = TRUE, {
      foo <- new_class("foo")
      foo(1)
      foo(1, 2)
      foo(x = 1)
      foo(x = 1, y = 2)
    })
  })


  it("can use the parent constructor to instantiate objects", {
    text2 <- new_class("text2", parent = "character")
    my_class <- new_class("my_class", parent = text2, properties = c("name" = "character"), constructor = function(x, name) new_object(text2(x), name = name))

    obj <- my_class("foo", "bar")

    expect_equal(obj@.data, text2("foo"))
  })
})

test_that("printing R7 objects work", {
  x <- range(1, 10)

  expect_snapshot(print(x))
})

test_that("printing R7 classes work", {
  expect_snapshot(range)
})

test_that("str with simple R7 objects work", {
  expect_snapshot(str(range(1, 2)), transform = scrub_src_references)
})

test_that("str with R7 objects of base classes work", {
  expect_snapshot(str(list(text("b"), number(50))), transform = scrub_src_references)
})

test_that("str R7 classes work", {
  expect_snapshot(str(range), transform = scrub_src_references)
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
