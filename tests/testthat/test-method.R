describe("method registration", {
  it("adds methods to the generic", {
    foo <- new_generic("foo", "x")
    method(foo, "character") <- function(x) "c"
    method(foo, "integer") <- function(x) "i"
    expect_length(methods(foo), 2)
  })

  it("adds method for each element of a union", {
    foo <- new_generic("foo", "x")
    method(foo, "numeric") <- function(x) "x"

    # one method for each union component
    expect_length(methods(foo), 2)

    # each method has the expected signature
    expect_equal(method(foo, "integer")@signature, list(as_class("integer")))
    expect_equal(method(foo, "double")@signature, list(as_class("double")))
  })

  it("can register method for external generic from within package", {
    on.exit(external_methods_reset("R7"), add = TRUE)

    foo <- new_external_generic("foo", "bar")
    register_external_method(foo, "character", function(x, ...) "bar", package = "R7")
    expect_length(external_methods_get("R7"), 1)

    # and doesn't modify generic
    expect_s3_class(foo, "R7_external_generic")
  })

  it("can register method for external generic during development", {
    bar <- new_class("bar")
    base_sum <- new_external_generic("base", "sum")
    register_external_method(base_sum, bar, function(x, ...) "bar", package = NULL)
    expect_equal(sum(bar()), "bar")
  })

  it("can register R7 method for S3 generic", {
    foo <- new_class("foo")
    method(sum, foo) <- function(x, ...) "foo"
    expect_equal(sum(foo()), "foo")

    # and doesn't modify generic
    expect_equal(sum, base::sum)
  })

  it("S3 registration requires a R7 class", {
    foo <- new_class("foo")
    expect_snapshot(error = TRUE, {
      method(sum, new_S3_class("foo")) <- function(x, ...) "foo"
    })
  })

  it("can register R7 method for S4 generic", {
    methods::setGeneric("bar", function(x) standardGeneric("bar"))
    foo <- new_class("foo")
    method(bar, foo) <- function(x) "foo"

    expect_equal(bar(foo()), "foo")
  })

  it("checks argument types", {
    foo <- new_generic("foo", "x")
    expect_snapshot(error = TRUE, {
      x <- 10
      method(x, "character") <- function(x) ...
      method(foo, 1) <- function(x) ...
    })
  })
})

describe("as_signature()", {
  it("returns a list that matches length of dispatch args", {
    foo1 <- new_generic("foo1", "x")
    sig1 <- as_signature("numeric", foo1)
    expect_type(sig1, "list")
    expect_length(sig1, 1)

    foo2 <- new_generic("foo2", c("x", "y"))
    sig2 <- as_signature(list("numeric", "character"), foo2)
    expect_type(sig2, "list")
    expect_length(sig2, 2)
  })

  it("forbids list for single dispatch", {
    foo <- new_generic("foo", "x")
    expect_snapshot(as_signature(list(1), foo), error = TRUE)
  })

  it("requires a list of the correct length for multiple dispatch", {
    foo <- new_generic("foo", c("x", "y"))
    expect_snapshot(error = TRUE, {
      as_signature("character", foo)
      as_signature(list("character"), foo)
    })
  })

})

test_that("check_method returns TRUE if the functions are compatible", {
  foo <- new_generic("foo", fun = function(x, ...) method_call())
  expect_true(check_method(function(x, ...) x, foo))
  # extra arguments are ignored
  expect_true(check_method(function(x, ..., y) x, foo))

  foo <- new_generic("foo", fun = function(x) method_call())
  expect_true(check_method(function(x) x, foo))
})

test_that("check_method errors if the functions are not compatible", {
  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", "x")
    check_method(1, foo)
    check_method(function(y) {}, foo)
    check_method(function(x = "foo") {}, foo)
    check_method(function(x, y, ...) {}, foo)
  })
})

test_that("check_method warn if default arguments don't match", {
  expect_snapshot({
    foo <- new_generic("foo", fun = function(x, ..., z = 2, y = 1) method_call())
    check_method(function(x, ..., y = 1) {}, foo)
    check_method(function(x, ..., y = 1, z = 1) {}, foo)
  })
})

test_that("R7_method printing", {
  foo <- new_generic("foo", c("x", "y"))
  method(foo, list(text, "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot(
    method(foo, list(text, "integer")),
    transform = scrub_environment
  )
})
