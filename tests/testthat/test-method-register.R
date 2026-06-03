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
    foo1 <- new_class("foo")
    method(sum, foo1) <- function(x, ...) "foo"
    expect_equal(sum(foo1()), "foo")

    foo2 <- new_class("foo", package = "bar")
    method(sum, foo2) <- function(x, ...) "foo"
    expect_equal(sum(foo2()), "foo")

    # and doesn't modify generic
    expect_equal(sum, base::sum)
  })

  it("can register S7 method for S3 Ops generic", {
    foo <- new_class("foo")
    bar <- new_class("bar")

    method(`+`, list(foo, bar)) <- function(e1, e2) "foobar"
    expect_equal(foo() + bar(), "foobar")

    if (getRversion() >= "4.3.0") {
      method(`%*%`, list(foo, bar)) <- function(x, y) "foo.bar"
      expect_equal(foo() %*% bar(), "foo.bar")
    }
  })

  it("can register S7 method for S3 generic defined in a local environment", {
    s3_gen <- local(function(x) UseMethod("s3_gen"))
    defer(unregister_s3_methods(topenv(environment(s3_gen)), "s3_gen"))

    local({
      method(s3_gen, class_character) <- function(x) "char"
      method(s3_gen, class_integer) <- function(x) "int"
    })

    expect_equal(s3_gen("a"), "char")
    expect_equal(s3_gen(1L), "int")
  })

  it("can register S7 method for S3 generic with base type signature", {
    local_s3_generic("s3_gen")
    method(s3_gen, class_character) <- function(x) "char"
    method(s3_gen, class_integer) <- function(x) "int"

    expect_equal(s3_gen("a"), "char")
    expect_equal(s3_gen(1L), "int")
  })

  it("can register S7 method for S3 generic with S3 class signature", {
    local_s3_generic("s3_gen")
    method(s3_gen, new_S3_class("foo")) <- function(x) "foo"
    method(s3_gen, class_factor) <- function(x) "factor"

    expect_equal(s3_gen(structure(list(), class = "foo")), "foo")
    expect_equal(s3_gen(factor("a")), "factor")
  })

  it("S3 registration for a multi-class S3 class uses only the first class", {
    local_s3_generic("s3_gen")
    method(s3_gen, new_S3_class(c("ordered", "factor"))) <- function(x) "ord"

    expect_equal(s3_gen(ordered("a")), "ord")
    # plain factors don't match because only `ordered` was registered
    expect_error(s3_gen(factor("a")), "no applicable method")
  })

  it("can register S7 method for S3 generic with class_any and NULL", {
    local_s3_generic("s3_gen")
    method(s3_gen, class_any) <- function(x) "any"
    method(s3_gen, NULL) <- function(x) "null"

    expect_equal(s3_gen(1L), "any")
    expect_equal(s3_gen(NULL), "null")
  })

  it("S3 method registration expands unions to one method per class", {
    local_s3_generic("s3_gen")
    method(s3_gen, class_numeric) <- function(x) "num"

    expect_equal(s3_gen(1L), "num")
    expect_equal(s3_gen(1.5), "num")

    # Custom union mixing a base type and an S3 class
    local_s3_generic("s3_gen2")
    method(s3_gen2, class_character | new_S3_class("foo")) <- function(x) "x"

    expect_equal(s3_gen2("a"), "x")
    expect_equal(s3_gen2(structure(list(), class = "foo")), "x")
  })

  it("rejects class_missing on S3 generics", {
    local_s3_generic("s3_gen")
    expect_snapshot(error = TRUE, {
      method(s3_gen, class_missing) <- function(x) "missing"
    })
  })

  it("can register S7 method for S4 generic", {
    methods::setGeneric("bar", function(x) standardGeneric("bar"))
    S4foo <- new_class("S4foo", package = NULL)

    expect_snapshot_error(method(bar, S4foo) <- function(x) "foo")

    S4_register(S4foo)
    on.exit(S4_remove_classes("S4foo"), add = TRUE)

    method(bar, S4foo) <- function(x) "foo"
    expect_equal(bar(S4foo()), "foo")
  })

  it("can register S4 methods for base types, class_any, class_missing, and NULL", {
    methods::setGeneric("s4_gen", function(x) standardGeneric("s4_gen"))
    method(s4_gen, class_character) <- function(x) "char"
    method(s4_gen, class_any) <- function(x) "any"
    method(s4_gen, class_missing) <- function(x) "missing"
    method(s4_gen, NULL) <- function(x) "null"

    expect_equal(s4_gen("hi"), "char")
    expect_equal(s4_gen(list()), "any")
    expect_equal(s4_gen(), "missing")
    expect_equal(s4_gen(NULL), "null")
  })

  it("S4 method registration expands S7 unions to one method per class", {
    methods::setGeneric("s4_union", function(x) standardGeneric("s4_union"))
    method(s4_union, class_integer | class_character) <- function(x) "u"

    expect_equal(s4_union(1L), "u")
    expect_equal(s4_union("a"), "u")
  })

  it("S4 method registration on class_double catches actual doubles", {
    # class(1.5) is "numeric", not "double", so class_double must register
    # under S4's "numeric" class to dispatch on real doubles.
    methods::setGeneric("s4_double", function(x) standardGeneric("s4_double"))
    method(s4_double, class_numeric) <- function(x) "num"

    expect_equal(s4_double(1L), "num")
    expect_equal(s4_double(1.5), "num")
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

describe("method unregistration", {
  it("removes S7 method via NULL assignment", {
    foo <- new_generic("foo", "x")
    method(foo, class_character) <- function(x) "c"
    method(foo, class_integer) <- function(x) "i"
    expect_length(methods(foo), 2)

    method(foo, class_character) <- NULL
    expect_length(methods(foo), 1)
    expect_equal(foo(1L), "i")
    expect_snapshot(foo("x"), error = TRUE)
  })

  it("removes each method in a union signature", {
    foo <- new_generic("foo", "x")
    method(foo, class_numeric) <- function(x) "n"
    expect_length(methods(foo), 2)

    method(foo, class_numeric) <- NULL
    expect_length(methods(foo), 0)
  })

  it("removes method with multi-dispatch signature", {
    foo <- new_generic("foo", c("x", "y"))
    A <- new_class("A")
    B <- new_class("B")
    method(foo, list(A, B)) <- function(x, y) "AB"
    expect_equal(foo(A(), B()), "AB")

    method(foo, list(A, B)) <- NULL
    expect_snapshot(foo(A(), B()), error = TRUE)
  })

  it("is a silent no-op when the method doesn't exist", {
    foo <- new_generic("foo", "x")
    expect_silent(method(foo, class_character) <- NULL)
    expect_length(methods(foo), 0)
  })

  it("errors when unregistering from an S3 generic", {
    foo <- new_class("foo")
    method(sum, foo) <- function(x, ...) "foo"
    expect_snapshot(method(sum, foo) <- NULL, error = TRUE)

    # External generics that resolve to S3 generics also error
    base_sum <- new_external_generic("base", "sum", "x")
    expect_snapshot(method(base_sum, foo) <- NULL, error = TRUE)
  })

  it("errors when unregistering from an S4 generic", {
    methods::setGeneric("removeS4", function(x) standardGeneric("removeS4"))
    on.exit(suppressMessages(methods::removeGeneric("removeS4")), add = TRUE)
    S4foo <- new_class("S4foo", package = NULL)
    S4_register(S4foo)
    on.exit(S4_remove_classes("S4foo"), add = TRUE)

    method(removeS4, S4foo) <- function(x) "foo"
    expect_snapshot(method(removeS4, S4foo) <- NULL, error = TRUE)
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

  it("accepts a length-1 list for single dispatch (#555)", {
    foo <- new_generic("foo", "x")
    sig <- as_signature(list(class_character), foo)
    expect_s3_class(sig, "S7_signature")
    expect_equal(sig, as_signature(class_character, foo))

    # but a list with the wrong contents still errors
    expect_snapshot(as_signature(list(1), foo), error = TRUE)
  })

  it("requires a list of the correct length for multiple dispatch", {
    foo <- new_generic("foo", c("x", "y"))
    expect_snapshot(error = TRUE, {
      as_signature(class_character, foo)
      as_signature(list(class_character), foo)
    })
  })

  it("works with NULL", {
    foo <- new_generic("foo", c("x"))
    sig <- as_signature(NULL, foo)
    expect_length(sig, 1)

    foo <- new_generic("foo", c("x", "y", "z"))
    sig <- as_signature(list(NULL, NULL, class_integer), foo)
    expect_length(sig, 3)
  })
})

test_that("S7_signature has format and print methods", {
  foo <- new_generic("foo", c("x", "y"))
  sig <- as_signature(list(class_integer, class_character), foo)

  expect_equal(format(sig), "<integer>, <character>")
  expect_snapshot(print(sig))
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

test_that("check_method rejects primitive functions", {
  expect_snapshot(error = TRUE, {
    foo <- new_generic("foo", "x")
    check_method(log, foo)
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
  method(foo, list(class_integer, class_integer)) <- function(x, y, ...) {
    paste0("bar:", x, y)
  }
  expect_snapshot(
    method(foo, list(class_integer, class_integer)),
    transform = scrub_environment
  )
})
