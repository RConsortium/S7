test_that("can register S7 method for S3 generic", {
  foo1 := new_class()
  method(sum, foo1) <- function(x, ...) "foo"
  expect_equal(sum(foo1()), "foo")

  foo2 := new_class(package = "bar")
  method(sum, foo2) <- function(x, ...) "foo"
  expect_equal(sum(foo2()), "foo")

  # and wraps the base generic with a sentinel
  expect_equal(sum, generic_sentinel(sum))
})

test_that("can register S7 method for S3 Ops generic", {
  foo := new_class()
  bar := new_class()

  method(`+`, list(foo, bar)) <- function(e1, e2) "foobar"
  expect_equal(foo() + bar(), "foobar")

  if (getRversion() >= "4.3.0") {
    method(`%*%`, list(foo, bar)) <- function(x, y) "foo.bar"
    expect_equal(foo() %*% bar(), "foo.bar")
  }
})

test_that("can register S7 method for S3 generic defined in a local environment", {
  s3_gen <- local(function(x) UseMethod("s3_gen"))
  defer(unregister_s3_methods(topenv(environment(s3_gen)), "s3_gen"))

  local({
    method(s3_gen, class_character) <- function(x) "char"
    method(s3_gen, class_integer) <- function(x) "int"
  })

  expect_equal(s3_gen("a"), "char")
  expect_equal(s3_gen(1L), "int")
})

test_that("can register S7 method for S3 generic with base type signature", {
  local_s3_generic("s3_gen")
  method(s3_gen, class_character) <- function(x) "char"
  method(s3_gen, class_integer) <- function(x) "int"

  expect_equal(s3_gen("a"), "char")
  expect_equal(s3_gen(1L), "int")
})

test_that("can register S7 method for S3 generic with S3 class signature", {
  local_s3_generic("s3_gen")
  method(s3_gen, new_S3_class("foo")) <- function(x) "foo"
  method(s3_gen, class_factor) <- function(x) "factor"

  expect_equal(s3_gen(structure(list(), class = "foo")), "foo")
  expect_equal(s3_gen(factor("a")), "factor")
})

test_that("internal generics register S4 methods for S4-backed S7 classes", {
  on.exit({
    try(methods::removeMethod("dim", "S4regDimParent"), silent = TRUE)
    try(methods::removeMethod("dim", "S4regDimChild"), silent = TRUE)
    S4_remove_classes(c(
      "S4regDimParent",
      "S4regDimChild",
      "S4regDimShim"
    ))
  })

  setClass("S4regDimParent", contains = "VIRTUAL")
  setMethod("dim", "S4regDimParent", function(x) {
    stop("parent S4 method should not be called", call. = FALSE)
  })
  S4regDimChild <- new_class(
    "S4regDimChild",
    parent = getClass("S4regDimParent"),
    properties = list(x = class_integer),
    package = NULL
  )
  method(dim, S4regDimChild) <- function(x) c(x@x, 2L)
  S4regDimChild_S4 <- S4_contains(S4regDimChild)
  setClass("S4regDimShim", contains = S4regDimChild_S4)

  object <- methods::new("S4regDimShim", x = 1L)

  expect_true(isS4(object))
  expect_equal(dim(object), c(1L, 2L))
})

test_that("base closures register S4 methods for S4-backed S7 classes", {
  defer({
    suppressWarnings(try(
      methods::removeMethod("unlist", "S4regUnlistChild"),
      silent = TRUE
    ))
    S4_remove_classes(c(
      "S4regUnlistParent",
      "S4regUnlistChild",
      "S4regUnlistShim"
    ))
  })

  setClass("S4regUnlistParent", contains = "VIRTUAL")
  S4regUnlistChild := new_class(
    parent = getClass("S4regUnlistParent"),
    properties = list(x = class_integer),
    package = NULL
  )
  method(unlist, S4regUnlistChild) <- function(
    x,
    recursive = TRUE,
    use.names = TRUE
  ) {
    x@x
  }
  S4regUnlistChild_S4 <- S4_contains(S4regUnlistChild)
  setClass("S4regUnlistShim", contains = S4regUnlistChild_S4)

  object <- methods::new("S4regUnlistShim", x = 1L)

  expect_equal(unlist(object), 1L)
  expect_true(methods::hasMethod("unlist", "S4regUnlistChild"))
})

test_that("local S3 generics named like internal generics don't register S4 methods", {
  defer({
    suppressWarnings(try(
      methods::removeMethod("dim", "S4LocalDimChild"),
      silent = TRUE
    ))
    S4_remove_classes(c(
      "S4LocalDimParent",
      "S4LocalDimChild"
    ))
  })

  setClass("S4LocalDimParent", contains = "VIRTUAL")
  S4LocalDimChild := new_class(
    parent = getClass("S4LocalDimParent"),
    properties = list(x = class_integer),
    package = NULL
  )
  dim <- local(function(x) UseMethod("dim"))
  defer(unregister_s3_methods(topenv(environment(dim)), "dim"))

  method(dim, S4LocalDimChild) <- function(x) c(x@x, 2L)

  expect_equal(dim(S4LocalDimChild(x = 1L)), c(1L, 2L))
  expect_null(methods::selectMethod(
    "dim",
    "S4LocalDimChild",
    optional = TRUE,
    useInherited = FALSE
  ))
})

test_that("internal replacement generics can register full S4 signatures", {
  on.exit({
    try(
      methods::removeMethod(
        "dimnames<-",
        c("S4regDimnamesChild", "list")
      ),
      silent = TRUE
    )
    S4_remove_classes(c(
      "S4regDimnamesParent",
      "S4regDimnamesChild",
      "S4regDimnamesShim"
    ))
  })

  setClass("S4regDimnamesParent", contains = "VIRTUAL")
  S4regDimnamesChild <- new_class(
    "S4regDimnamesChild",
    parent = getClass("S4regDimnamesParent"),
    properties = list(x = class_list),
    package = NULL
  )
  method(`dimnames<-`, list(S4regDimnamesChild, class_list)) <-
    function(x, value) {
      x@x <- value
      x
    }
  S4regDimnamesChild_S4 <- S4_contains(S4regDimnamesChild)
  setClass("S4regDimnamesShim", contains = S4regDimnamesChild_S4)

  object <- methods::new("S4regDimnamesShim", x = list(NULL, NULL))
  value <- list("r", "c")
  dimnames(object) <- value

  expect_equal(methods::slot(object, "x"), value)
  expect_true(methods::hasMethod(
    "dimnames<-",
    c("S4regDimnamesChild", "list")
  ))
})

test_that("sentinels for internal replacement generics keep full S4 signatures", {
  on.exit(S4_remove_classes(c(
    "S4regDimnamesSentinelParent",
    "S4regDimnamesSentinelChild"
  )))

  setClass("S4regDimnamesSentinelParent", contains = "VIRTUAL")
  S4regDimnamesSentinelChild <- new_class(
    "S4regDimnamesSentinelChild",
    parent = getClass("S4regDimnamesSentinelParent"),
    properties = list(x = class_list),
    package = NULL
  )

  dimnames_sentinel <- generic_sentinel(as_generic(`dimnames<-`))
  expect_no_error(
    register_method(
      dimnames_sentinel,
      list(S4regDimnamesSentinelChild, class_list),
      function(x, value) x,
      package = NULL
    )
  )
  on.exit(
    methods::removeMethod(
      "dimnames<-",
      c("S4regDimnamesSentinelChild", "list")
    ),
    add = TRUE,
    after = FALSE
  )
  expect_true(methods::hasMethod(
    "dimnames<-",
    c("S4regDimnamesSentinelChild", "list")
  ))
})

test_that("S3 registration for a multi-class S3 class uses only the first class", {
  local_s3_generic("s3_gen")
  method(s3_gen, new_S3_class(c("ordered", "factor"))) <- function(x) "ord"

  expect_equal(s3_gen(ordered("a")), "ord")
  # plain factors don't match because only `ordered` was registered
  expect_error(s3_gen(factor("a")), "no applicable method")
})

test_that("can register S7 method for S3 generic with class_any and NULL", {
  local_s3_generic("s3_gen")
  method(s3_gen, class_any) <- function(x) "any"
  method(s3_gen, NULL) <- function(x) "null"

  expect_equal(s3_gen(1L), "any")
  expect_equal(s3_gen(NULL), "null")
})

test_that("S3 method registration expands unions to one method per class", {
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

test_that("rejects class_missing on S3 generics", {
  local_s3_generic("s3_gen")
  expect_snapshot(error = TRUE, {
    method(s3_gen, class_missing) <- function(x) "missing"
  })
})

test_that("errors when unregistering from an S3 generic", {
  foo := new_class()
  method(sum, foo) <- function(x, ...) "foo"
  expect_snapshot(method(sum, foo) <- NULL, error = TRUE)

  # External generics that resolve to S3 generics also error
  base_sum <- new_external_generic("base", "sum", "x")
  expect_snapshot(method(base_sum, foo) <- NULL, error = TRUE)
})
