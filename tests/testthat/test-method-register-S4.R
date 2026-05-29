describe("S4 method registration", {
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
})

describe("S4 method unregistration", {
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
