describe("method registration", {
  it("adds methods to the generic", {
    foo <- new_generic("foo", dispatch_args = "x")
    method(foo, "character") <- function(x) "c"
    method(foo, "integer") <- function(x) "i"
    expect_length(methods(foo), 2)
  })

  it("adds method for each element of a union", {
    foo <- new_generic("foo", dispatch_args = "x")
    method(foo, "numeric") <- function(x) "x"

    # one method for each union component
    expect_length(methods(foo), 2)

    # each method has the expected signature
    expect_equal(method(foo, "integer")@signature, as_signature("integer"))
    expect_equal(method(foo, "double")@signature, as_signature("double"))
  })

  it("can register method for external generic from within package", {
    on.exit(external_methods_reset("R7"), add = TRUE)

    foo <- new_external_generic("foo", "bar")
    register_method(foo, "character", function(x, ...) "bar", package = "R7")
    expect_length(external_methods_get("R7"), 1)

    # and doesn't modify generic
    expect_s3_class(foo, "R7_external_generic")
  })

  it("can register method for external generic during development", {
    bar <- new_class("bar")
    base_sum <- new_external_generic("base", "sum")
    register_method(base_sum, bar, function(x, ...) "bar", package = NULL)
    expect_equal(sum(bar()), "bar")
  })

  it("can register R7 method for S3 generic", {
    foo <- new_class("foo")
    method(sum, foo) <- function(x, ...) "foo"
    expect_equal(sum(foo()), "foo")

    # and doesn't modify generic
    expect_equal(sum, base::sum)
  })

  it("S3 registration requires single R7 class", {
    foo <- new_class("foo")
    expect_snapshot(error = TRUE, {
      method(sum, list(foo, foo)) <- function(x, ...) "foo"
      method(sum, s3_class("foo")) <- function(x, ...) "foo"
    })
  })

  it("checks argument types", {
    foo <- new_generic("foo", dispatch_args = "x")
    expect_snapshot(error = TRUE, {
      x <- 10
      method(x, "character") <- function(x) ...
      method(foo, 1) <- function(x) ...
    })
  })
})


test_that("check_method returns TRUE if the functions are compatible", {
  foo <- new_generic("foo", function(x, ...) method_call())
  expect_true(check_method(function(x, ...) x, "character", foo))
  # extra arguments are ignored
  expect_true(check_method(function(x, ..., y) x, "character", foo))

  foo <- new_generic("foo", function(x) method_call())
  expect_true(check_method(function(x) x, "character", foo))
})

test_that("check_method errors if the functions are not compatible", {
  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", dispatch_args = "x")
    check_method(1, "character", foo)
    check_method(function(y) {}, "character", foo)
    check_method(function(x = "foo") {}, "character", foo)
    check_method(function(x, y, ...) {}, "character", foo)
  })
})

test_that("check_method warn if default arguments don't match", {
  expect_snapshot({
    foo <- new_generic("foo", function(x, ..., z = 2, y = 1) method_call())
    check_method(function(x, ..., y = 1) {}, "character", foo)
    check_method(function(x, ..., y = 1, z = 1) {}, "character", foo)
  })
})

test_that("R7_method printing", {
  foo <- new_generic(name="foo", dispatch_args = c("x", "y"))
  method(foo, list(text, "integer")) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot(
    method(foo, list(text, "integer")),
    transform = scrub_environment
  )
})
