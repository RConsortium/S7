test_that("Ops generics dispatch to S7 methods for S7 classes", {
  local_methods(base_ops[["+"]])
  foo1 := new_class()
  foo2 := new_class()

  method(`+`, list(foo1, foo1)) <- function(e1, e2) "foo1-foo1"
  method(`+`, list(foo1, foo2)) <- function(e1, e2) "foo1-foo2"
  method(`+`, list(foo2, foo1)) <- function(e1, e2) "foo2-foo1"
  method(`+`, list(foo2, foo2)) <- function(e1, e2) "foo2-foo2"

  expect_equal(foo1() + foo1(), "foo1-foo1")
  expect_equal(foo1() + foo2(), "foo1-foo2")
  expect_equal(foo2() + foo1(), "foo2-foo1")
  expect_equal(foo2() + foo2(), "foo2-foo2")

  expect_error(
    foo1() + new_class("foo3")(),
    class = "S7_error_method_not_found"
  )
})

test_that("Ops generics dispatch to S3 methods", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])
  defer(unregister_s3_methods(baseenv(), "Ops"))

  foo := new_class()
  method(`+`, list(class_factor, foo)) <- function(e1, e2) "factor-foo"
  method(`+`, list(foo, class_factor)) <- function(e1, e2) "foo-factor"

  expect_equal(foo() + factor(), "foo-factor")
  expect_equal(factor() + foo(), "factor-foo")

  # Even if custom method exists
  foo_S3 <- structure(list(), class = "foo_S3")
  local_s3_method("+.foo_S3", function(e1, e2) stop("Failure!"))

  method(`+`, list(new_S3_class("foo_S3"), foo)) <- function(e1, e2) "S3-S7"
  method(`+`, list(foo, new_S3_class("foo_S3"))) <- function(e1, e2) "S7-S3"

  expect_equal(foo() + foo_S3, "S7-S3")
  expect_equal(foo_S3 + foo(), "S3-S7")
})

test_that("operator methods on S3/S4 classes work when neither operand is S7", {
  local_methods(base_ops[["+"]])
  local_S4_classes()

  class_foo <- new_S3_class("foo")
  foo <- structure(list(), class = "foo")
  method(`+`, list(class_foo, class_any)) <- function(e1, e2) "foo+any"
  expect_equal(foo + 10, "foo+any")

  fooS4 <- setClass("fooS4", contains = "character")
  method(`+`, list(fooS4, class_any)) <- function(e1, e2) "fooS4+any"
  expect_equal(fooS4("x") + 10, "fooS4+any")

  # An unregistered operator still falls back to the base behaviour
  expect_error(foo * 10, regexp = "non-numeric argument")
})

test_that("operator bridge does not clobber an existing group method", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])
  defer(unregister_s3_methods(baseenv(), "Ops"))

  local_s3_method("Ops.myS3", function(e1, e2) "myS3-ops")
  method(`+`, list(new_S3_class("myS3"), class_any)) <- function(e1, e2) "!"

  # + method used for S7 classes
  x <- structure(list(), class = "myS3")
  foo := new_class()
  expect_equal(x + foo(), "!")

  # but `Ops.myS3` used for base/S3 classes
  expect_equal(x + 10, "myS3-ops")
})

test_that("Ops generics dispatch to S7 methods for S4 classes", {
  local_methods(base_ops[["+"]])
  local_S4_classes()
  fooS4 <- setClass("fooS4", contains = "character")
  fooS7 := new_class()

  method(`+`, list(fooS7, fooS4)) <- function(e1, e2) "S7-S4"
  method(`+`, list(fooS4, fooS7)) <- function(e1, e2) "S4-S7"

  expect_equal(fooS4() + fooS7(), "S4-S7")
  expect_equal(fooS7() + fooS4(), "S7-S4")
})

test_that("Ops generics dispatch to S7 methods for POSIXct", {
  # In R's C sources DispatchGroup() has special cases for POSIXt/Date/difftime
  # so we need to double check that S7 methods still take precedence:
  # https://github.com/wch/r-source/blob/5cc4e46fc/src/main/eval.c#L4242C1-L4247C64

  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])
  foo := new_class()

  method(`+`, list(foo, class_POSIXct)) <- function(e1, e2) "foo-POSIXct"
  expect_equal(foo() + Sys.time(), "foo-POSIXct")

  method(`+`, list(class_POSIXct, foo)) <- function(e1, e2) "POSIXct-foo"
  expect_equal(Sys.time() + foo(), "POSIXct-foo")
})

test_that("Ops generics dispatch to S7 methods for NULL", {
  local_methods(base_ops[["+"]])
  foo := new_class()

  method(`+`, list(foo, NULL)) <- function(e1, e2) "foo-NULL"
  method(`+`, list(NULL, foo)) <- function(e1, e2) "NULL-foo"

  expect_equal(foo() + NULL, "foo-NULL")
  expect_equal(NULL + foo(), "NULL-foo")
})

test_that("Ops generics falls back to base behaviour", {
  local_methods(base_ops[["+"]])

  foo := new_class(parent = class_double)
  expect_equal(+foo(1), foo(+1))
  expect_equal(foo(1) + 1, foo(2))
  expect_equal(foo(1) + 1:2, 2:3)
  expect_equal(1 + foo(1), foo(2))
  expect_equal(1:2 + foo(1), 2:3)

  # but can be overridden
  method(`+`, list(foo, class_numeric)) <- function(e1, e2) "foo-numeric"
  method(`+`, list(class_numeric, foo)) <- function(e1, e2) "numeric-foo"
  expect_equal(foo(1) + 1, "foo-numeric")
  expect_equal(foo(1) + 1:2, "foo-numeric")
  expect_equal(1 + foo(1), "numeric-foo")
  expect_equal(1:2 + foo(1), "numeric-foo")

  method(`+`, list(foo, class_missing)) <- function(e1, e2) "foo"
  expect_equal(+foo(), "foo")
})

test_that("`%*%` dispatches to S7 methods", {
  skip_if(getRversion() < "4.3")
  local_methods(base_ops[["+"]])

  ClassX := new_class()
  method(`%*%`, list(ClassX, class_any)) <- function(x, y) {
    "ClassX %*% class_any"
  }
  method(`%*%`, list(class_any, ClassX)) <- function(x, y) {
    "class_any %*% ClassX"
  }

  expect_equal(ClassX() %*% ClassX(), "ClassX %*% class_any")
  expect_equal(ClassX() %*% 1, "ClassX %*% class_any")
  expect_equal(1 %*% ClassX(), "class_any %*% ClassX")
})

test_that("Ops methods can use super", {
  foo := new_class(class_integer)
  foo2 := new_class(foo)

  method(`+`, list(foo, class_double)) <- function(e1, e2) {
    foo(S7_data(e1) + as.integer(e2))
  }
  method(`+`, list(foo2, class_double)) <- function(e1, e2) {
    foo2(super(e1, foo) + e2)
  }

  expect_equal(foo2(1L) + 1, foo2(2L))
})


test_that("unary and binary Ops methods dispatch independently (#531)", {
  local_methods(base_ops[["+"]], base_ops[["-"]])
  Foo := new_class()
  Child := new_class(parent = Foo)

  method(`+`, list(Foo, class_missing)) <- function(e1, e2) {
    expect_identical(missing(e2), TRUE)
    "plus Foo"
  }
  method(`-`, list(Foo, class_missing)) <- function(e1, e2) {
    expect_identical(missing(e2), TRUE)
    "minus Foo"
  }
  method(`+`, list(Foo, Foo)) <- \(e1, e2) "Foo plus Foo"
  method(`-`, list(Foo, Foo)) <- \(e1, e2) "Foo minus Foo"

  expect_identical(+Foo(), "plus Foo")
  expect_identical(-Foo(), "minus Foo")
  expect_identical(+Child(), "plus Foo")
  expect_identical(-Child(), "minus Foo")
  expect_identical(Foo() + Foo(), "Foo plus Foo")
  expect_identical(Foo() - Foo(), "Foo minus Foo")
})

test_that("unary Ops methods can use super", {
  local_methods(base_ops[["-"]])
  Number := new_class(parent = class_double)
  Child := new_class(parent = Number)
  method(`-`, list(Number, class_missing)) <- function(e1, e2) {
    Number(-S7_data(e1))
  }
  method(`-`, list(Child, class_missing)) <- function(e1, e2) {
    Child(-super(e1, Number))
  }

  expect_identical(-Child(1), Child(-1))
})

test_that("Ops methods propagate missing-method errors from their bodies", {
  local_methods(base_ops[["+"]], base_ops[["-"]], base_ops[["!"]])
  Number := new_class(parent = class_double)
  Flag := new_class(parent = class_logical)
  other := new_generic("x")
  method(`+`, list(Number, class_missing)) <- \(e1, e2) other(e1)
  method(`-`, list(Number, class_missing)) <- \(e1, e2) other(e1)
  method(`+`, list(Number, class_double)) <- \(e1, e2) other(e1)
  method(`!`, Flag) <- \(e1) other(e1)

  expect_snapshot(error = TRUE, +Number(1))
  expect_snapshot(error = TRUE, -Number(1))
  expect_snapshot(error = TRUE, Number(1) + 1)
  expect_snapshot(error = TRUE, !Flag(TRUE))
})

test_that("Ops methods propagate missing-method errors from the same operator", {
  local_methods(base_ops[["-"]])
  Number := new_class(parent = class_double)
  Other := new_class()
  method(`-`, list(Number, class_missing)) <- \(e1, e2) Other() - Other()

  expect_snapshot(error = TRUE, -Number(1))
})

test_that("Ops methods preserve restarts when propagating method errors", {
  local_methods(base_ops[["-"]])
  Number := new_class(parent = class_double)
  other := new_generic("x")
  method(`-`, list(Number, class_missing)) <- function(e1, e2) {
    withRestarts(other(e1), recover = \() "recovered")
  }

  out <- withCallingHandlers(
    -Number(1),
    S7_error_method_not_found = \(cnd) invokeRestart("recover")
  )
  expect_identical(out, "recovered")
})

test_that("Ops methods preserve NULL results and base operator visibility", {
  local_methods(base_ops[["+"]], base_ops[["-"]], base_ops[["!"]])
  Number := new_class(parent = class_double)
  Flag := new_class(parent = class_logical)
  method(`+`, list(Number, class_missing)) <- \(e1, e2) NULL
  method(`-`, list(Number, class_missing)) <- \(e1, e2) invisible(NULL)
  method(`+`, list(Number, Number)) <- \(e1, e2) invisible(3)
  method(`!`, Flag) <- \(e1) invisible(FALSE)

  expect_identical(withVisible(+Number(1)), list(value = NULL, visible = TRUE))
  expect_identical(withVisible(-Number(1)), list(value = NULL, visible = TRUE))
  expect_identical(
    withVisible(Number(1) + Number(2)),
    list(value = 3, visible = TRUE)
  )
  expect_identical(
    withVisible(!Flag(TRUE)),
    list(value = FALSE, visible = TRUE)
  )
})

test_that("unary Ops fall back when no method is registered", {
  local_methods(base_ops[["+"]], base_ops[["-"]])
  Number := new_class(parent = class_double)
  method(`+`, list(Number, Number)) <- \(e1, e2) "binary"
  method(`-`, list(Number, Number)) <- \(e1, e2) "binary"

  expect_identical(+Number(1), Number(1))
  expect_identical(-Number(1), Number(-1))
})

test_that("unary Ops dispatch for S3 and S4 classes", {
  local_methods(base_ops[["+"]], base_ops[["-"]])
  local_S4_classes()
  defer(unregister_s3_methods(baseenv(), "Ops"))
  NumberS3 <- new_S3_class("NumberS3")
  NumberS4 <- setClass("NumberS4", contains = "numeric")

  method(`+`, list(NumberS3, class_missing)) <- \(e1, e2) "plus S3"
  method(`-`, list(NumberS3, class_missing)) <- \(e1, e2) "minus S3"
  method(`+`, list(NumberS4, class_missing)) <- \(e1, e2) "plus S4"
  method(`-`, list(NumberS4, class_missing)) <- \(e1, e2) "minus S4"

  expect_identical(+structure(1, class = "NumberS3"), "plus S3")
  expect_identical(-structure(1, class = "NumberS3"), "minus S3")
  expect_identical(+NumberS4(1), "plus S4")
  expect_identical(-NumberS4(1), "minus S4")
})

test_that("packages can unload and reload unary operator methods", {
  local_methods(base_ops[["+"]], base_ops[["-"]], base_ops[["!"]])
  operators := local_package({
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    Number := new_class(parent = class_double)
    Flag := new_class(parent = class_logical)
    method(`+`, list(Number, class_missing)) <- \(e1, e2) "plus"
    method(`-`, list(Number, class_missing)) <- \(e1, e2) "minus"
    method(`!`, Flag) <- \(e1) "not"
    S7_on_build()
  })

  operators$.onUnload()
  expect_identical(+operators$Number(1), operators$Number(1))
  expect_identical(-operators$Number(1), operators$Number(-1))
  expect_identical(!operators$Flag(TRUE), operators$Flag(FALSE))

  # Clear session registrations independently before testing the load hook.
  method(`!`, operators$Flag) <- NULL
  operators$.onLoad()
  expect_identical(+operators$Number(1), "plus")
  expect_identical(-operators$Number(1), "minus")
  expect_identical(!operators$Flag(TRUE), "not")
})

test_that("installed packages register unary methods in a fresh session", {
  skip_if(quick_test())
  local_dev_S7_lib()
  lib <- local_libpath()
  quick_install(test_path("unaryops"), lib)

  result <- callr::r(
    function() {
      options(warn = 2)
      ns <- loadNamespace("unaryops")
      number <- ns$Number(1)
      flag <- ns$Flag(TRUE)
      loaded <- c(+number, -number, !flag)
      unloadNamespace("unaryops")
      unloaded <- c(
        S7::S7_data(+number),
        S7::S7_data(-number),
        S7::S7_data(!flag)
      )
      loadNamespace("unaryops")
      reloaded <- c(+number, -number, !flag)
      list(loaded = loaded, unloaded = unloaded, reloaded = reloaded)
    },
    libpath = .libPaths()
  )

  expect_identical(
    result,
    list(
      loaded = c("plus", "minus", "not"),
      unloaded = c(1, -1, 0),
      reloaded = c("plus", "minus", "not")
    )
  )
})

test_that("`!` dispatches on a single argument", {
  local_methods(base_ops[["!"]])

  Logical := new_class(class_logical)
  method(`!`, Logical) <- function(e1) Logical(!as.logical(e1))

  expect_identical(!Logical(TRUE), Logical(FALSE))
  method(`!`, Logical) <- NULL
  method(`!`, list(Logical)) <- \(e1) "length-1 list"
  expect_identical(!Logical(TRUE), "length-1 list")
})

test_that("`!` requires a length-1 signature", {
  local_methods(base_ops[["!"]])

  Logical := new_class(class_logical)
  expect_snapshot(error = TRUE, {
    method(`!`, list(Logical, class_missing)) <- function(e1, e2) e1
  })
})

test_that("`!` can use super", {
  local_methods(base_ops[["!"]])

  Logical := new_class(class_logical)
  Logical2 := new_class(Logical)
  method(`!`, Logical) <- function(e1) "Logical"
  method(`!`, Logical2) <- function(e1) paste0(!super(e1, Logical), "2")

  expect_equal(!Logical2(TRUE), "Logical2")
})

test_that("`!` dispatches to S7 methods for S3 and S4 classes", {
  local_methods(base_ops[["!"]])
  local_S4_classes()
  defer(unregister_s3_methods(baseenv(), "Ops"))

  method(`!`, new_S3_class("myS3")) <- function(e1) "myS3"
  expect_equal(!structure(TRUE, class = "myS3"), "myS3")

  fooS4 <- setClass("fooS4", contains = "logical")
  method(`!`, fooS4) <- function(e1) "fooS4"
  expect_equal(!fooS4(TRUE), "fooS4")
})

test_that("`!` falls back to base behaviour", {
  local_methods(base_ops[["!"]], base_ops[["+"]])
  defer(unregister_s3_methods(baseenv(), "Ops"))

  foo := new_class(parent = class_logical)
  expect_identical(!foo(TRUE), foo(FALSE))

  method(`+`, list(foo, class_any)) <- function(e1, e2) "foo-any"
  expect_identical(!foo(TRUE), foo(FALSE))

  # An S3 bridge installed for a binary operator also catches `!`.
  method(`+`, list(new_S3_class("FlagS3"), class_any)) <- \(e1, e2) "S3-any"
  expect_identical(
    !structure(TRUE, class = "FlagS3"),
    structure(FALSE, class = "FlagS3")
  )
})
