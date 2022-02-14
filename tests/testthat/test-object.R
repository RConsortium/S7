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
  it("reports all property type errors", {
    expect_snapshot(range(start = "x", end = "y"), error = TRUE)
  })

  it("checks new objects for validity", {
    expect_error(range(start = 10, end = 1), "`end` must be greater than or equal to `start`")
  })

  it("can instantiate a new object that inherits from a basic type", {
    y <- text("foo")
    expect_equal(r7_data(y), as_class("character")("foo"))
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

  it("combines properties for parent classes", {
    foo1 <- new_class("foo1", properties = list(x = "numeric"))
    foo2 <- new_class("foo2", foo1, properties = list(y = "numeric"))
    expect_equal(names(foo2@properties), c("x", "y"))
  })
  it("child properties override parent", {
    foo1 <- new_class("foo1", properties = list(x = "numeric"))
    foo2 <- new_class("foo2", foo1, properties = list(x = "character"))
    expect_equal(names(foo2@properties), "x")
    expect_equal(foo2@properties$x$class, as_class("character"))
  })

  it("can use the parent constructor to instantiate objects", {
    text2 <- new_class("text2", parent = "character")
    my_class <- new_class("my_class",
      parent = text2,
      properties = list(name = "character"),
      constructor = function(x, name) new_object(text2(x), name = name)
    )
    obj <- my_class("foo", "bar")
    expect_equal(r7_data(obj), text2("foo"))
  })
})

test_that("print()/str() gives useful display", {
  expect_snapshot({
    str(range(1, 10))
    str(list(text("b"), number(50)))
  })
})
test_that("print()/str() nests properties correctly", {
  klass <- new_class("klass", properties = list(x = "numeric", y = range))

  expect_snapshot({
    str(klass(x = 10, y = range(1, 10)))
  })
})

test_that("object_class returns itself for R7_class objects", {
  text <- new_class("text", parent = "character")

  expect_equal(object_class(text), text)
})

test_that("object_class returns the object class property for R7_object objects", {
  text <- new_class("text", parent = "character")

  obj <- text("hi")

  expect_equal(object_class(obj), text)
})

test_that("object_class returns class for basic types", {
  expect_equal(object_class("foo"), "character")
})

test_that("object_class returns class for S3 types", {
  foo <- structure(list(), class = "foo")
  expect_equal(object_class(foo), "foo")
})

test_that("object_class returns the class for S4 types", {
  foo2 <- methods::setClass("foo2", representation = "character")
  obj <- foo2("hi")
  expect_equal(object_class(obj), methods::extends(class(obj)))
})

test_that("can inherit from an S3 class", {
  ordered2 <- new_class("ordered2", parent = s3_factor)
  x <- ordered2(c(1L, 2L, 1L), letters[1:3])
  expect_equal(class(x), c("ordered2", "R7_object", "factor"))
  expect_equal(prop_names(x), character())
  expect_error(x@levels, "Can't find property")
})
