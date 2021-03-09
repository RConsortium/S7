test_that("method will fall back to S3 generics if no R7 generic is defined", {
  expect_equal(
    method(print, list("text")),
    base::print.default
  )
})

test_that("method errors if no method is defined for that class", {
  foo <- generic_new(name = "foo", signature = alist(x=))

  expect_error(
    method(foo, list("blah")),
    "Can't find method for generic 'foo'"
  )
})

test_that("methods can be registered for a generic and then called", {
  foo <- generic_new(name = "foo", signature = alist(x=))
  method_new(foo, "text", function(x) paste0("foo-", x@.data))

  expect_equal(foo(text("bar")), "foo-bar")
})

test_that("single inheritance works when searching for methods", {
  foo2 <- generic_new(name = "foo2", signature = alist(x=))

  method_new(foo2, "character", function(x) paste0("foo2-", x))

  expect_equal(foo2(text("bar")), "foo2-bar")
})

test_that("direct multiple dispatch works", {
  foo3 <- generic_new(name = "foo3", signature = alist(x=, y=))
  method_new(foo3, list("text", "number"), function(x, y) paste0(x, y))
  expect_equal(foo3(text("bar"), number(1)), "bar1")
})

test_that("inherited multiple dispatch works", {
  foo4 <- generic_new(name = "foo4", signature = alist(x=, y=))
  method_new(foo4, list("character", "numeric"), function(x, y) paste0(x, ":", y))

  expect_equal(foo4(text("bar"), number(1)), "bar:1")
})

test_that("method dispatch works for S3 objects", {
  foo <- generic_new(name = "foo", signature = "x")

  obj <- structure("hi", class = "my_s3")

  method_new(foo, "my_s3", function(x) paste0("foo-", x))

  expect_equal(foo(obj), "foo-hi")
})

test_that("method dispatch works for S3 objects", {
  skip_if_not(requireNamespace("methods"))

  Range <- setClass("Range", slots = c(start = "numeric", end = "numeric"))
  obj <- Range(start = 1, end = 10)

  foo <- generic_new(name = "foo", signature = "x")

  method_new(foo, "Range", function(x) paste0("foo-", x@start, "-", x@end))

  expect_equal(foo(obj), "foo-1-10")
})

test_that("method_new works if you use r7 class objects", {
  foo5 <- generic_new(name = "foo5", signature = alist(x=, y=))
  method_new(foo5, list(text, number), function(x, y) paste0(x, ":", y))

  expect_equal(foo5(text("bar"), number(1)), "bar:1")
})

test_that("method_new works if you pass a bare class", {
  foo6 <- generic_new(name = "foo6", signature = alist(x=))
  method_new(foo6, text, function(x) paste0("foo-", x))

  expect_equal(foo6(text("bar")), "foo-bar")
})

test_that("method_new works if you pass a bare class union", {
  foo7 <- generic_new(name = "foo7", signature = alist(x=))
  method_new(foo7, class_union(text, number), function(x) paste0("foo-", x))

  expect_equal(foo7(text("bar")), "foo-bar")
  expect_equal(foo7(number(1)), "foo-1")
})

test_that("method_next works for single dispatch", {
  foo <- generic_new("foo", "x")

  method_new(foo, "text", function(x) {
    x@.data <- paste0("foo-", x@.data)
    method_next(foo, list(object_class(x)))(x)
  })

  method_new(foo, "character", function(x) {
    as.character(x)
  })

  expect_equal(foo(text("hi")), "foo-hi")
})

test_that("method_next works for double dispatch", {
  foo <- generic_new("foo", c("x", "y"))

  method_new(foo, list("text", "number"), function(x, y) {
    x@.data <- paste0("foo-", x@.data, "-", y@.data)
    method_next(foo, list(object_class(x), object_class(y)))(x, y)
  })

  method_new(foo, list("character", "number"), function(x, y) {
    y@.data <- y + 1
    x@.data <- paste0(x@.data, "-", y@.data)
    method_next(foo, list(object_class(x), object_class(y)))(x, y)
  })

  method_new(foo, list("character", "numeric"), function(x, y) {
    as.character(x@.data)
  })

  expect_equal(foo(text("hi"), number(1)), "foo-hi-1-2")
})

test_that("method_new errors if given a length > 1 character vector", {
  foo <- generic_new("foo", "x")

  expect_error(
    method_new(c("foo", "bar"), "x", function(x) x),
    "must be a generic function or a length 1 character vector"
  )
})

test_that("method_new works with both hard and soft dependencies", {
  skip_on_os("windows")

  tmp_lib <- tempfile()
  dir.create(tmp_lib)
  old_libpaths <- .libPaths()
  .libPaths(c(tmp_lib, old_libpaths))
  on.exit({
    .libPaths(old_libpaths)
    detach("package:t2", unload = TRUE)
    detach("package:t1", unload = TRUE)
    detach("package:t0", unload = TRUE)
    unlink(tmp_lib, recursive = TRUE)
  })

  quick_install(test_path(c("t0", "t1", "t2")))

  library("t2")

  # t2 has a soft dependency on t1
  library("t1")
  expect_equal(foo("blah", 1), "foo-blah-1")

  # t2 has a hard dependency on t0
  library("t0")
  expect_equal(bar("blah", 1), "bar-blah-1")
})
