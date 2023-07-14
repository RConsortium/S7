describe("method registration", {
  it("adds methods to the generic", {
    foo <- new_generic("foo", "x")
    method(foo, class_character) <- function(x) "c"
    method(foo, class_integer) <- function(x) "i"
    expect_length(methods(foo), 2)
  })

  it("adds messages when overwriting", {
    foo <- new_generic("foo", "x")
    expect_snapshot({
      method(foo, class_character) <- function(x) "c"
      method(foo, class_character) <- function(x) "c"
    })
    expect_length(methods(foo), 1)
  })

  it("adds method for each element of a union", {
    foo <- new_generic("foo", "x")
    method(foo, class_numeric) <- function(x) "x"

    # one method for each union component
    expect_length(methods(foo), 2)

    # each method has the expected signature
    expect_equal(method(foo, class_integer)@signature, list(class_integer))
    expect_equal(method(foo, class_double)@signature, list(class_double))
  })

  it("can register method for external generic", {
    bar <- new_class("bar")
    base_sum <- new_external_generic("base", "sum", "x")

    method(base_sum, bar) <- function(x, ...) "bar"
    expect_equal(sum(bar()), "bar")

    # and doesn't modify generic
    expect_equal(sum, base::sum)
  })

  it("can register S7 method for S3 generic", {
    foo <- new_class("foo")
    method(sum, foo) <- function(x, ...) "foo"
    expect_equal(sum(foo()), "foo")

    # and doesn't modify generic
    expect_equal(sum, base::sum)
  })

  it("can register S7 method for S3 Ops generic", {
    foo <- new_class("foo")
    bar <- new_class("bar")

    method(`+`, list(foo, bar)) <- function(x, y) "foobar"
    expect_equal(foo() + bar(), "foobar")

    if(getRversion() >= "4.3.0") {
      method(`%*%`, list(foo, bar)) <- function(x, y) "foo.bar"
      expect_equal(foo() %*% bar(), "foo.bar")
    }
  })

  it("S3 registration requires a S7 class", {
    foo <- new_class("foo")
    expect_snapshot(error = TRUE, {
      method(sum, new_S3_class("foo")) <- function(x, ...) "foo"
    })
  })

  it("can register S7 method for S4 generic", {
    methods::setGeneric("bar", function(x) standardGeneric("bar"))
    S4foo <- new_class("S4foo")

    expect_snapshot_error(method(bar, S4foo) <- function(x) "foo")

    S4_register(S4foo)
    on.exit(S4_remove_classes("S4foo"), add = TRUE)

    method(bar, S4foo) <- function(x) "foo"
    expect_equal(bar(S4foo()), "foo")
  })

  it("checks argument types", {
    foo <- new_generic("foo", "x")
    expect_snapshot(error = TRUE, {
      x <- 10
      method(x, class_character) <- function(x) ...
      method(foo, 1) <- function(x) ...
    })
  })
})

describe("as_signature()", {
  it("returns a list that matches length of dispatch args", {
    foo1 <- new_generic("foo1", "x")
    sig1 <- as_signature(class_numeric, foo1)
    expect_s3_class(sig1, "S7_signature")
    expect_length(sig1, 1)

    foo2 <- new_generic("foo2", c("x", "y"))
    sig2 <- as_signature(list(class_numeric, class_character), foo2)
    expect_s3_class(sig1, "S7_signature")
    expect_length(sig2, 2)
  })

  it("is idempotent", {
    expect_equal(as_signature(new_signature(10)), new_signature(10))
  })

  it("forbids list for single dispatch", {
    foo <- new_generic("foo", "x")
    expect_snapshot(as_signature(list(1), foo), error = TRUE)
  })

  it("requires a list of the correct length for multiple dispatch", {
    foo <- new_generic("foo", c("x", "y"))
    expect_snapshot(error = TRUE, {
      as_signature(class_character, foo)
      as_signature(list(class_character), foo)
    })
  })
})

test_that("check_method returns TRUE if the functions are compatible", {
  foo <- new_generic("foo", "x", function(x, ...) S7_dispatch())
  expect_true(check_method(function(x, ...) x, foo))
  # extra arguments are ignored
  expect_true(check_method(function(x, ..., y) x, foo))

  foo <- new_generic("foo", "x", function(x) S7_dispatch())
  expect_true(check_method(function(x) x, foo))
})

test_that("check_method complains if the functions are not compatible", {
  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", "x")
    check_method(1, foo)
    check_method(function(y) {}, foo)
    check_method(function(x = "foo") {}, foo)
    check_method(function(x, y, ...) {}, foo)
  })

  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", "x", function(x) S7_dispatch())
    check_method(function(x, y) {}, foo)
  })
})

test_that("check_method warn if default arguments don't match", {
  expect_snapshot({
    foo <- new_generic("foo", "x", function(x, ..., z = 2, y = 1) S7_dispatch())
    check_method(function(x, ..., y = 1) {}, foo)
    check_method(function(x, ..., y = 1, z = 1) {}, foo)
  })
})

test_that("S7_method printing", {
  foo <- new_generic("foo", c("x", "y"))
  method(foo, list(class_integer, class_integer)) <- function(x, y, ...) paste0("bar:", x, y)
  expect_snapshot(
    method(foo, list(class_integer, class_integer)),
    transform = scrub_environment
  )
})
