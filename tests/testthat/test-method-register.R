describe("method registration", {
  it("adds methods to the generic", {
    foo := new_generic("x")
    method(foo, class_character) <- function(x) "c"
    method(foo, class_integer) <- function(x) "i"
    expect_length(methods(foo), 2)
  })

  it("adds messages when overwriting", {
    foo := new_generic("x")
    expect_snapshot({
      method(foo, class_character) <- function(x) "c"
      method(foo, class_character) <- function(x) "C"
    })
    expect_length(methods(foo), 1)
  })

  it("re-registering an identical method is silent (#474)", {
    foo <- new_generic("foo", "x")
    fn <- function(x) "c"
    method(foo, class_character) <- fn
    expect_no_message(method(foo, class_character) <- fn)
    expect_length(methods(foo), 1)
  })

  it("adds method for each element of a union", {
    foo := new_generic("x")
    method(foo, class_numeric) <- function(x) "x"

    # one method for each union component
    expect_length(methods(foo), 2)

    # each method has the expected signature
    expect_equal(method(foo, class_integer)@signature, list(class_integer))
    expect_equal(method(foo, class_double)@signature, list(class_double))
  })

  it("can register method for external generic", {
    bar := new_class()
    base_sum <- new_external_generic("base", "sum", "x")

    method(base_sum, bar) <- function(x, ...) "bar"
    expect_equal(sum(bar()), "bar")

    # and doesn't modify generic
    expect_equal(sum, base::sum)
  })

  it("checks argument types", {
    foo := new_generic("x")
    expect_snapshot(error = TRUE, {
      x <- 10
      method(x, class_character) <- function(x) ...
      method(foo, 1) <- function(x) ...
    })
  })

  it("returns the generic unchanged when not in a package (#364)", {
    foo := new_generic("x")
    out <- register_method(foo, class_integer, function(x) "i", package = NULL)
    expect_identical(out, foo)

    bar := new_class(package = NULL)
    out <- register_method(sum, bar, function(x, ...) "bar", package = NULL)
    expect_identical(out, sum)
  })

  it("returns a strippable sentinel for foreign generics in a package (#364)", {
    external_methods_reset("S7")
    on.exit(external_methods_reset("S7"), add = TRUE)

    foo := new_class(package = NULL)
    ext <- new_external_generic("notloaded.pkg", "ext_gen", "x")

    out <- register_method(
      ext,
      foo,
      function(x) "x",
      env = asNamespace("S7"),
      package = "S7"
    )
    expect_s3_class(out, "S7_generic_sentinel")
    expect_s3_class(out, "S7_external_generic")

    # the sentinel is still a usable generic, so further methods can be
    # registered through the same binding (as in the t2 test package)
    foo2 := new_class(package = NULL)
    out <- register_method(
      out,
      foo2,
      function(x) "y",
      env = asNamespace("S7"),
      package = "S7"
    )
    expect_s3_class(out, "S7_generic_sentinel")
    expect_length(S7_methods_table("S7"), 2)
  })
})

describe("method unregistration", {
  it("removes S7 method via NULL assignment", {
    foo := new_generic("x")
    method(foo, class_character) <- function(x) "c"
    method(foo, class_integer) <- function(x) "i"
    expect_length(methods(foo), 2)

    method(foo, class_character) <- NULL
    expect_length(methods(foo), 1)
    expect_equal(foo(1L), "i")
    expect_snapshot(foo("x"), error = TRUE)
  })

  it("removes each method in a union signature", {
    foo := new_generic("x")
    method(foo, class_numeric) <- function(x) "n"
    expect_length(methods(foo), 2)

    method(foo, class_numeric) <- NULL
    expect_length(methods(foo), 0)
  })

  it("removes method with multi-dispatch signature", {
    foo := new_generic(c("x", "y"))
    A := new_class()
    B := new_class()
    method(foo, list(A, B)) <- function(x, y) "AB"
    expect_equal(foo(A(), B()), "AB")

    method(foo, list(A, B)) <- NULL
    expect_snapshot(foo(A(), B()), error = TRUE)
  })

  it("is a silent no-op when the method doesn't exist", {
    foo := new_generic("x")
    expect_silent(method(foo, class_character) <- NULL)
    expect_length(methods(foo), 0)
  })
})

describe("as_signature()", {
  it("returns a list that matches length of dispatch args", {
    foo1 := new_generic("x")
    sig1 <- as_signature(class_numeric, foo1)
    expect_s3_class(sig1, "S7_signature")
    expect_length(sig1, 1)

    foo2 := new_generic(c("x", "y"))
    sig2 <- as_signature(list(class_numeric, class_character), foo2)
    expect_s3_class(sig1, "S7_signature")
    expect_length(sig2, 2)
  })

  it("is idempotent", {
    expect_equal(as_signature(new_signature(10)), new_signature(10))
  })

  it("accepts a length-1 list for single dispatch (#555)", {
    foo := new_generic("x")
    sig <- as_signature(list(class_character), foo)
    expect_s3_class(sig, "S7_signature")
    expect_equal(sig, as_signature(class_character, foo))

    # but a list with the wrong contents still errors
    expect_snapshot(as_signature(list(1), foo), error = TRUE)
  })

  it("requires a list of the correct length for multiple dispatch", {
    foo := new_generic(c("x", "y"))
    expect_snapshot(error = TRUE, {
      as_signature(class_character, foo)
      as_signature(list(class_character), foo)
    })
  })

  it("works with NULL", {
    foo := new_generic(c("x"))
    sig <- as_signature(NULL, foo)
    expect_length(sig, 1)

    foo := new_generic(c("x", "y", "z"))
    sig <- as_signature(list(NULL, NULL, class_integer), foo)
    expect_length(sig, 3)
  })
})

test_that("check_method returns TRUE if the functions are compatible", {
  foo := new_generic("x", function(x, ...) S7_dispatch())
  expect_true(check_method(function(x, ...) x, foo))
  # extra arguments are ignored
  expect_true(check_method(function(x, ..., y) x, foo))

  foo := new_generic("x", function(x) S7_dispatch())
  expect_true(check_method(function(x) x, foo))
})

test_that("check_method complains if the functions are not compatible", {
  expect_snapshot(error = TRUE, {
    foo := new_generic("x")
    check_method(1, foo)
    check_method(function(y) {}, foo)
    check_method(function(x = "foo") {}, foo)
    check_method(function(x, y, ...) {}, foo)
  })

  expect_snapshot(error = TRUE, {
    foo := new_generic("x", function(x) S7_dispatch())
    check_method(function(x, y) {}, foo)
  })
})

test_that("check_method rejects primitive functions", {
  expect_snapshot(error = TRUE, {
    foo := new_generic("x")
    check_method(log, foo)
  })
})

test_that("check_method warn if default arguments don't match", {
  expect_snapshot({
    foo := new_generic("x", function(x, ..., z = 2, y = 1) S7_dispatch())
    check_method(function(x, ..., y = 1) {}, foo)
    check_method(function(x, ..., y = 1, z = 1) {}, foo)
  })
})

test_that("S7_method printing", {
  foo := new_generic(c("x", "y"))
  method(foo, list(class_integer, class_integer)) <- function(x, y, ...) {
    paste0("bar:", x, y)
  }
  expect_snapshot(
    method(foo, list(class_integer, class_integer)),
    transform = scrub_environment
  )
})
