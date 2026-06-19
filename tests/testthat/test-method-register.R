test_that("method registration adds methods to the generic", {
  foo := new_generic("x")
  method(foo, class_character) <- function(x) "c"
  method(foo, class_integer) <- function(x) "i"
  expect_length(methods(foo), 2)
})

test_that("method registration adds messages when overwriting", {
  foo := new_generic("x")
  expect_snapshot({
    method(foo, class_character) <- function(x) "c"
    method(foo, class_character) <- function(x) "C"
  })
  expect_length(methods(foo), 1)
})

test_that("re-registering an identical method is silent (#474)", {
  foo := new_generic("x")
  fn <- function(x) "c"
  method(foo, class_character) <- fn
  expect_no_message(method(foo, class_character) <- fn)
  expect_length(methods(foo), 1)
})

test_that("method registration adds a method for each element of a union", {
  foo := new_generic("x")
  method(foo, class_numeric) <- function(x) "x"

  # one method for each union component
  expect_length(methods(foo), 2)

  # each method has the expected signature
  expect_equal(method(foo, class_integer)@signature, list(class_integer))
  expect_equal(method(foo, class_double)@signature, list(class_double))
})

test_that("method registration can register a method for an external generic", {
  bar := new_class()
  base_sum <- new_external_generic("base", "sum", "x")

  method(base_sum, bar) <- function(x, ...) "bar"
  expect_equal(sum(bar()), "bar")

  # and doesn't modify generic
  expect_equal(sum, base::sum)
})

test_that("method registration checks argument types", {
  foo := new_generic("x")
  expect_snapshot(error = TRUE, {
    x <- 10
    method(x, class_character) <- function(x) ...
    method(foo, 1) <- function(x) ...
  })
})

test_that("method registration returns the generic unchanged when not in a package (#364)", {
  foo := new_generic("x")
  out <- register_method(foo, class_integer, function(x) "i", package = NULL)
  expect_identical(out, foo)

  bar := new_class(package = NULL)
  out <- register_method(sum, bar, function(x, ...) "bar", package = NULL)
  expect_identical(out, sum)
})

test_that("method registration resolves external classes outside packages", {
  env <- new.env(parent = baseenv())
  env[["method<-"]] <- `method<-`
  env$g <- new_generic("g", "x")
  env$ext <- new_external_class("S7", "S7_object")
  env$f <- function(x) "external"

  expect_null(packageName(env))
  evalq(method(g, ext) <- f, env)

  expect_equal(env$g(S7_object()), "external")

  evalq(method(g, ext) <- NULL, env)
  expect_snapshot(env$g(S7_object()), error = TRUE)
})

test_that("method registration returns a strippable sentinel for foreign generics in a package (#364)", {
  pkg := local_package(
    ext := new_external_generic("notloaded.pkg", "x"),
    foo := new_class(),
    foo2 := new_class()
  )

  # In a package, `method<-` writes a sentinel back into the binding
  evalq(method(ext, foo) <- function(x) "x", pkg)
  expect_s3_class(pkg$ext, "S7_generic_sentinel")
  expect_s3_class(pkg$ext, "S7_external_generic")

  # the sentinel is still a usable generic, so further methods can be
  # registered through the same binding (as in the t2 test package)
  evalq(method(ext, foo2) <- function(x) "y", pkg)
  expect_s3_class(pkg$ext, "S7_generic_sentinel")
  expect_length(S7_methods_table("pkg"), 2)
})

test_that("deferred external-class methods preserve sentinel for foreign generics", {
  pkg := local_package(
    ext := new_external_class("notloaded.pkg")
  )

  # In a package, `method<-` writes a sentinel back into the binding
  evalq(method(sum, ext) <- function(x, ...) "x", pkg)
  expect_s3_class(pkg$sum, "S7_generic_sentinel")
  expect_s3_class(pkg$sum, "S7_external_generic")
  expect_length(S7_methods_table("pkg"), 1)
})

test_that("deferred external-class methods match sentinel foreign generics", {
  pkg := local_package(
    gen := new_external_generic("notloaded.pkg", "x"),
    ext := new_external_class("notloaded.pkg")
  )

  evalq(method(gen, ext) <- function(x) "first", pkg)
  expect_s3_class(pkg$gen, "S7_generic_sentinel")

  evalq(method(gen, ext) <- function(x) "second", pkg)
  expect_length(S7_methods_table("pkg"), 1)
  expect_equal(S7_methods_table("pkg")[[1]]$method(NULL), "second")

  evalq(method(gen, ext) <- NULL, pkg)
  expect_length(S7_methods_table("pkg"), 0)
})

test_that("method registration defers external classes in union signatures", {
  pkg := local_package(
    foo := new_generic("x"),
    ext := new_external_class("notloaded.pkg"),
    method(foo, NULL | ext) <- function(x) "x"
  )

  expect_length(methods(pkg$foo), 0)
  expect_length(S7_methods_table("pkg"), 1)
})

test_that("method registration validates deferred external-class methods", {
  expect_snapshot(error = TRUE, {
    local_package(
      "pkg_invalid_deferred_external_class_method",
      foo := new_generic("x"),
      ext := new_external_class("notloaded.pkg"),
      method(foo, ext) <- function(y) "x"
    )
  })
})

test_that("method unregistration removes deferred external-class methods", {
  pkg := local_package(
    foo := new_generic("x"),
    ext := new_external_class("notloaded.pkg"),
    method(foo, ext) <- function(x) "x"
  )
  expect_length(S7_methods_table("pkg"), 1)

  evalq(method(foo, ext) <- NULL, pkg)
  expect_length(S7_methods_table("pkg"), 0)
})

test_that("method unregistration resolves loaded external-class methods in packages", {
  downstream <- local_package(
    "downstream_external_unregister_resolve",
    .onLoad <- function(...) S7_on_load(),
    foo := new_generic("x"),
    Ext <- new_external_class("S7", "S7_object"),
    method(foo, Ext) <- function(x) "external"
  )
  downstream$.onLoad()
  expect_equal(downstream$foo(S7_object()), "external")

  evalq(method(foo, Ext) <- NULL, downstream)
  expect_snapshot(downstream$foo(S7_object()), error = TRUE)

  downstream$.onLoad()
  expect_snapshot(downstream$foo(S7_object()), error = TRUE)
})

test_that("method unregistration ignores external class version constraints", {
  upstream <- local_package(
    "upstream_external_versioned_unregister",
    Ext := new_class()
  )
  downstream <- local_package(
    "downstream_external_versioned_unregister",
    .onLoad <- function(...) S7_on_load(),
    foo := new_generic(dispatch_args = "x"),
    ExtVersioned <- new_external_class(
      package = "upstream_external_versioned_unregister",
      name = "Ext",
      version = "0.0.0"
    ),
    method(foo, ExtVersioned) <- function(x) "versioned"
  )
  downstream$.onLoad()
  expect_equal(downstream$foo(upstream$Ext()), "versioned")

  evalq(
    {
      Ext <- new_external_class(
        package = "upstream_external_versioned_unregister",
        name = "Ext"
      )
      method(foo, Ext) <- NULL
    },
    downstream
  )
  expect_equal(nrow(S7_methods(generic = downstream$foo)), 0)

  downstream$.onLoad()
  expect_equal(nrow(S7_methods(generic = downstream$foo)), 0)
})

test_that("method unregistration removes deferred unions regardless of order", {
  upstream <- local_package(
    "upstream_external_union_unregister",
    Ext := new_class()
  )
  downstream <- local_package(
    "downstream_external_union_unregister",
    .onLoad <- function(...) S7_on_load(),
    foo := new_generic("x"),
    Ext := new_external_class("upstream_external_union_unregister"),
    method(foo, NULL | Ext) <- function(x) "external"
  )
  downstream$.onLoad()
  expect_equal(downstream$foo(upstream$Ext()), "external")

  evalq(method(foo, Ext | NULL) <- NULL, downstream)
  expect_snapshot(downstream$foo(upstream$Ext()), error = TRUE)

  downstream$.onLoad()
  expect_snapshot(downstream$foo(upstream$Ext()), error = TRUE)
})

test_that("method unregistration removes an S7 method via NULL assignment", {
  foo := new_generic("x")
  method(foo, class_character) <- function(x) "c"
  method(foo, class_integer) <- function(x) "i"
  expect_length(methods(foo), 2)

  method(foo, class_character) <- NULL
  expect_length(methods(foo), 1)
  expect_equal(foo(1L), "i")
  expect_snapshot(foo("x"), error = TRUE)
})

test_that("method unregistration removes each method in a union signature", {
  foo := new_generic("x")
  method(foo, class_numeric) <- function(x) "n"
  expect_length(methods(foo), 2)

  method(foo, class_numeric) <- NULL
  expect_length(methods(foo), 0)
})

test_that("method unregistration removes a method with a multi-dispatch signature", {
  foo := new_generic(c("x", "y"))
  A := new_class()
  B := new_class()
  method(foo, list(A, B)) <- function(x, y) "AB"
  expect_equal(foo(A(), B()), "AB")

  method(foo, list(A, B)) <- NULL
  expect_snapshot(foo(A(), B()), error = TRUE)
})

test_that("method unregistration is a silent no-op when the method doesn't exist", {
  foo := new_generic("x")
  expect_silent(method(foo, class_character) <- NULL)
  expect_length(methods(foo), 0)
})

test_that("as_signature() returns a list that matches length of dispatch args", {
  foo1 := new_generic("x")
  sig1 <- as_signature(class_numeric, foo1)
  expect_s3_class(sig1, "S7_signature")
  expect_length(sig1, 1)

  foo2 := new_generic(c("x", "y"))
  sig2 <- as_signature(list(class_numeric, class_character), foo2)
  expect_s3_class(sig1, "S7_signature")
  expect_length(sig2, 2)
})

test_that("as_signature() is idempotent", {
  expect_equal(as_signature(new_signature(10)), new_signature(10))
})

test_that("as_signature() accepts a length-1 list for single dispatch (#555)", {
  foo := new_generic("x")
  sig <- as_signature(list(class_character), foo)
  expect_s3_class(sig, "S7_signature")
  expect_equal(sig, as_signature(class_character, foo))

  # but a list with the wrong contents still errors
  expect_snapshot(as_signature(list(1), foo), error = TRUE)
})

test_that("as_signature() requires a list of the correct length for multiple dispatch", {
  foo := new_generic(c("x", "y"))
  expect_snapshot(error = TRUE, {
    as_signature(class_character, foo)
    as_signature(list(class_character), foo)
  })
})

test_that("as_signature() works with NULL", {
  foo := new_generic(c("x"))
  sig <- as_signature(NULL, foo)
  expect_length(sig, 1)

  foo := new_generic(c("x", "y", "z"))
  sig <- as_signature(list(NULL, NULL, class_integer), foo)
  expect_length(sig, 3)
})

test_that("S7_signature has format and print methods", {
  foo <- new_generic("foo", c("x", "y"))
  sig <- as_signature(list(class_integer, class_character), foo)

  expect_equal(format(sig), "<integer>, <character>")
  expect_snapshot(print(sig))
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
