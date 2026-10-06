test_that("S7_class validates its underlying data", {
  x <- new_class("X", package = NULL)()
  expect_snapshot_error(S7_data(x) <- 1)
})

test_that("$ gives useful error", {
  foo := new_class()
  x <- foo()
  expect_snapshot(error = TRUE, {
    x$y
    x$y <- 1
  })

  # But works as expected if inheriting from list
  foo := new_class(class_list)
  x <- foo()
  x$x <- 1
  expect_equal(x$x, 1)
})

test_that("[ gives more accurate error", {
  expect_snapshot(error = TRUE, {
    x <- new_class("foo")()
    x[1]
    x[1] <- 1
  })

  # but ok if inheriting from list
  x <- new_class("foo", class_list)()
  x[1] <- 1
  expect_equal(x[1], list(1))
})

test_that("[[ gives more accurate error", {
  expect_snapshot(error = TRUE, {
    x <- new_class("foo")()
    x[[1]]
    x[[1]] <- 1
  })

  # but ok if inheriting from list
  x <- new_class("foo", class_list)()
  x[[1]] <- 1
  expect_equal(x[[1]], 1)
})

test_that("register S4 classes for key components", {
  for (class in c("S7_object", "S7_method", "S7_generic")) {
    expect_s4_class(
      methods::getClassDef(class, package = "S7", inherits = FALSE),
      "classRepresentation"
    )
  }
})

test_that("tracing survives S7 reloads in a base-only session", {
  expect_null(callr::r(
    function() {
      options(warn = 2)
      stopifnot(!isNamespaceLoaded("methods"))
      loadNamespace("S7")
      stopifnot(isNamespaceLoaded("methods"))
      unloadNamespace("S7")
      stopifnot(is.null(methods::getClassDef("S7_generic")))

      library(S7)
      generic := new_generic("x")
      method(generic, class_integer) <- function(x) x
      original <- generic
      original_method <- method(generic, class_integer)
      table <- prop(generic, "methods")
      suppressMessages(trace(
        "integer",
        quote(NULL),
        print = FALSE,
        where = table
      ))
      suppressMessages(trace(
        "generic",
        quote(NULL),
        print = FALSE,
        where = environment()
      ))
      stopifnot(identical(generic(1L), 1L))
      suppressMessages(untrace("integer", where = table))
      suppressMessages(untrace("generic", where = environment()))
      stopifnot(
        identical(generic, original),
        identical(method(generic, class_integer), original_method)
      )
      detach("package:S7", unload = TRUE)

      ordinary <- function(x) x
      suppressMessages(trace(
        "ordinary",
        quote(NULL),
        print = FALSE,
        where = environment()
      ))
      stopifnot(identical(ordinary(1L), 1L))
      suppressMessages(untrace("ordinary", where = environment()))
      NULL
    },
    libpath = .libPaths(),
    env = c(R_DEFAULT_PACKAGES = "base")
  ))
})

test_that("S7 methods can be traced", {
  my_generic := new_generic("x")
  my_class := new_class(package = NULL)
  method(my_generic, my_class) <- function(x) "result"
  original <- method(my_generic, my_class)
  expect_identical(attr(original, "_S7_version", exact = TRUE), 1L)
  obj <- my_class()

  calls <- new.env()
  calls$n <- 0
  tracer <- function() calls$n <- calls$n + 1

  suppressMessages(
    trace("my_class", tracer, print = FALSE, where = my_generic@methods)
  )
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
  traced <- method(my_generic, my_class)
  expect_identical(traced@generic, original@generic)
  expect_identical(traced@signature, original@signature)
  expect_identical(S7_class(traced), S7_class(original))
  expect_identical(attr(traced, "_S7_version", exact = TRUE), 1L)
  expect_identical(methods::validObject(traced), TRUE)
  expect_output(
    print(method(my_generic, my_class)),
    "<S7_method>",
    fixed = TRUE
  )

  suppressMessages(untrace("my_class", where = my_generic@methods))
  expect_identical(method(my_generic, my_class), original)
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
})

test_that("S7 generics can be traced", {
  my_generic := new_generic("x")
  my_class := new_class(package = NULL)
  method(my_generic, my_class) <- function(x) "result"
  original <- my_generic
  expect_identical(attr(original, "_S7_version", exact = TRUE), 1L)
  original_method <- method(my_generic, my_class)
  obj <- my_class()

  calls <- new.env()
  calls$n <- 0
  tracer <- function() calls$n <- calls$n + 1

  suppressMessages(
    trace("my_generic", tracer, print = FALSE, where = environment())
  )
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
  expect_identical(my_generic@name, original@name)
  expect_identical(my_generic@dispatch_args, original@dispatch_args)
  expect_identical(my_generic@methods, original@methods)
  expect_identical(S7_class(my_generic), S7_class(original))
  expect_identical(attr(my_generic, "_S7_version", exact = TRUE), 1L)
  expect_identical(methods::validObject(my_generic), TRUE)
  expect_identical(method(my_generic, my_class), original_method)
  expect_identical(method(my_generic, object = obj), original_method)
  expect_output(print(my_generic), "<S7_generic>", fixed = TRUE)

  suppressMessages(untrace("my_generic", where = environment()))
  expect_identical(my_generic, original)
  expect_equal(my_generic(obj), "result")
  expect_equal(calls$n, 1)
})

test_that("generics and methods can be traced together with multiple dispatch", {
  my_generic := new_generic(c("x", "y"))
  my_class := new_class(package = NULL)
  method(my_generic, list(my_class, class_integer)) <- function(x, y) y
  original <- my_generic
  original_method <- method(my_generic, list(my_class, class_integer))
  obj <- my_class()
  calls <- new.env()
  calls$seen <- character()
  table <- my_generic@methods$my_class

  suppressMessages(trace(
    "integer",
    quote(calls$seen <- c(calls$seen, "method")),
    print = FALSE,
    where = table
  ))
  suppressMessages(trace(
    "my_generic",
    quote(calls$seen <- c(calls$seen, "generic")),
    print = FALSE,
    where = environment()
  ))

  traced <- method(my_generic, list(my_class, class_integer))
  expect_identical(method(my_generic, object = list(obj, 1L)), traced)
  expect_identical(traced@generic, original)
  expect_identical(traced@signature, original_method@signature)
  expect_identical(my_generic(obj, 1L), 1L)
  expect_identical(calls$seen, c("generic", "method"))

  suppressMessages(untrace("integer", where = table))
  expect_identical(
    method(my_generic, list(my_class, class_integer)),
    original_method
  )
  expect_identical(my_generic(obj, 2L), 2L)
  expect_identical(calls$seen, c("generic", "method", "generic"))

  suppressMessages(untrace("my_generic", where = environment()))
  expect_identical(my_generic, original)
  expect_identical(my_generic(obj, 3L), 3L)
  expect_identical(calls$seen, c("generic", "method", "generic"))
})

test_that("S7_dispatch rejects an untraced function with an original attribute", {
  my_generic := new_generic("x")
  method(my_generic, class_integer) <- function(x) x
  fake <- unclass(my_generic)
  attr(fake, "original") <- my_generic

  expect_snapshot(error = TRUE, fake(1L))
})
