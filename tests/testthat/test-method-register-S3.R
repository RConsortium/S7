describe("S3 method registration", {
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
})

describe("S3 method unregistration", {
  it("errors when unregistering from an S3 generic", {
    foo <- new_class("foo")
    method(sum, foo) <- function(x, ...) "foo"
    expect_snapshot(method(sum, foo) <- NULL, error = TRUE)

    # External generics that resolve to S3 generics also error
    base_sum <- new_external_generic("base", "sum", "x")
    expect_snapshot(method(base_sum, foo) <- NULL, error = TRUE)
  })
})
