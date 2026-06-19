test_that("S7_classes() / S7_generics() inspect a single environment", {
  # Namespace: restricted to exports
  expect_equal(S7_classes(asNamespace("S7")), "S7_object")
  expect_equal(S7_generics(asNamespace("S7")), "convert")
})

test_that("default `env` is the caller's environment", {
  local({
    Foo <- new_class("Foo", package = NULL)
    Bar <- new_class("Bar", package = NULL)
    my_gen <- new_generic("my_gen", "x")

    expect_setequal(S7_classes(), c("Foo", "Bar"))
    expect_setequal(S7_generics(), "my_gen")
  })

  expect_setequal(S7_classes(), character())
  expect_setequal(S7_generics(), character())
})

test_that("S7_methods(generic) lists registered methods", {
  Foo <- new_class("Foo", package = NULL)
  Bar <- new_class("Bar", package = NULL)
  gen <- new_generic("gen", "x")
  method(gen, Foo) <- function(x) "foo"
  method(gen, Bar) <- function(x) "bar"

  res <- S7_methods(generic = gen)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("generic", "package", "signature"))
  expect_equal(res$generic, c("gen", "gen"))
  expect_setequal(vcapply(res$signature, format), c("<Foo>", "<Bar>"))
})

test_that("S7_methods() prints the signature column readably", {
  Foo <- new_class("Foo", package = NULL)
  Bar <- new_class("Bar", package = NULL)
  gen <- new_generic("gen", "x")
  method(gen, Foo) <- function(x) "foo"
  method(gen, Bar) <- function(x) "bar"

  expect_snapshot(print(S7_methods(generic = gen)))
})

test_that("S7_signature_list formats per element", {
  foo <- new_generic("foo", c("x", "y"))
  sigs <- new_signature_list(list(
    as_signature(list(class_integer, class_character), foo),
    as_signature(list(class_double, class_logical), foo)
  ))

  expect_equal(
    format(sigs),
    c("<integer>, <character>", "<double>, <logical>")
  )
})

test_that("S7_methods(generic) handles multi-dispatch", {
  Foo <- new_class("Foo", package = NULL)
  Bar <- new_class("Bar", package = NULL)
  gen <- new_generic("gen", c("x", "y"))
  method(gen, list(Foo, Bar)) <- function(x, y) "fb"

  res <- S7_methods(generic = gen)
  expect_equal(vcapply(res$signature, format), "<Foo>, <Bar>")
})

test_that("S7_methods(generic) returns empty df when no methods", {
  gen <- new_generic("gen", "x")
  res <- S7_methods(generic = gen)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_named(res, c("generic", "package", "signature"))
})

test_that("S7_methods(class) scans attached generics", {
  Foo <- new_class("Foo", package = NULL)
  Bar <- new_class("Bar", package = NULL)
  g1 <- new_generic("S7_introspect_g1_xyzzy", "x")
  g2 <- new_generic("S7_introspect_g2_xyzzy", "x")
  method(g1, Foo) <- function(x) "foo"
  method(g2, Bar) <- function(x) "bar"

  assign("S7_introspect_g1_xyzzy", g1, envir = globalenv())
  assign("S7_introspect_g2_xyzzy", g2, envir = globalenv())
  defer(rm(
    list = c("S7_introspect_g1_xyzzy", "S7_introspect_g2_xyzzy"),
    envir = globalenv()
  ))

  res <- S7_methods(class = Foo)
  expect_true("S7_introspect_g1_xyzzy" %in% res$generic)
  expect_false("S7_introspect_g2_xyzzy" %in% res$generic)
  expect_equal(
    res$package[res$generic == "S7_introspect_g1_xyzzy"],
    NA_character_
  )
})

test_that("S7_methods(class) uses real key for external S7_object", {
  gen_name <- "S7_introspect_s7_object_xyzzy"
  gen <- new_generic(name = gen_name, dispatch_args = "x")
  method(gen, S7_object) <- function(x) "s7"

  assign(gen_name, gen, envir = globalenv())
  defer(rm(list = gen_name, envir = globalenv()))

  res <- S7_methods(class = new_external_class("S7", "S7_object"))
  expect_equal(res$generic[res$generic == gen_name], gen_name)
})

test_that("S7_methods() reports the generic's package", {
  Foo <- new_class("Foo", package = NULL)
  gen <- new_generic("gen", "x")
  method(gen, Foo) <- function(x) "foo"

  res <- S7_methods(generic = gen)
  expect_equal(res$package, NA_character_)
})

test_that("S7_methods() validates inputs", {
  expect_snapshot(error = TRUE, {
    S7_methods(generic = "not a generic")
  })
})

test_that("find_objects() returns matching names", {
  env <- new.env(parent = emptyenv())
  env$Foo <- new_class("Foo", package = NULL)
  env$bar <- 1L
  env$Baz <- new_class("Baz", package = NULL)

  expect_setequal(find_objects(env, is_class), c("Foo", "Baz"))
  expect_setequal(find_objects(env, is.integer), "bar")
})
