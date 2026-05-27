test_that("new_external_class() validates inputs", {
  expect_snapshot(error = TRUE, {
    new_external_class(1, "x")
    new_external_class("pkg", 1)
  })
})

test_that("print method works", {
  expect_snapshot({
    print(new_external_class("foo", "Bar"))
    print(new_external_class("foo", "Bar", version = "1.0"))
  })
})

test_that("external class is a valid class spec", {
  ec <- new_external_class("foo", "Bar")

  expect_identical(as_class(ec), ec)
  expect_equal(class_type(ec), "S7_external")
  expect_equal(class_register(ec), "foo::Bar")
  expect_equal(class_desc(ec), "<foo::Bar>")
  expect_equal(S7_class_desc(ec), "<foo::Bar>")
})

test_that("external class can refer to a loaded package", {
  # S7 is itself loaded, S7_object exists in it
  ec <- new_external_class("S7", "S7_object")
  resolved <- resolve_external_class(ec)
  expect_true(is_class(resolved))
  expect_equal(resolved@name, "S7_object")
})

test_that("unresolved external class returns NULL", {
  expect_null(resolve_external_class(new_external_class("not_a_pkg", "X")))
  expect_null(resolve_external_class(new_external_class("S7", "not_a_class")))
})

test_that("external class can be used as a union arm", {
  ec <- new_external_class("foo", "Bar")
  u <- NULL | ec
  expect_s3_class(u, "S7_union")
  expect_length(u$classes, 2)
})

test_that("external class works as a property type for self-reference", {
  tree <- new_class(
    "tree",
    package = "mypkg",
    properties = list(
      label = class_character,
      child = NULL | new_external_class("mypkg", "tree")
    )
  )

  t1 <- tree(label = "leaf")
  expect_null(t1@child)

  t2 <- tree(label = "root", child = t1)
  expect_equal(t2@child@label, "leaf")

  # type checking still rejects wrong types
  expect_snapshot(error = TRUE, tree(label = "bad", child = 1))
})

test_that("external class works for mutually recursive classes", {
  class_one <- new_class(
    "class_one",
    package = "mypkg",
    properties = list(x = NULL | new_external_class("mypkg", "class_two"))
  )
  class_two <- new_class(
    "class_two",
    package = "mypkg",
    properties = list(y = NULL | new_external_class("mypkg", "class_one"))
  )

  obj <- class_one(x = class_two(y = class_one()))
  expect_s3_class(obj@x, "mypkg::class_two")
  expect_s3_class(obj@x@y, "mypkg::class_one")
})

test_that("class_inherits() works for external class", {
  tree <- new_class(
    "tree",
    package = "mypkg",
    properties = list(child = NULL | new_external_class("mypkg", "tree"))
  )
  ec <- new_external_class("mypkg", "tree")
  expect_true(class_inherits(tree(), ec))
  expect_false(class_inherits(1, ec))
  expect_false(class_inherits(NULL, ec))
})

test_that("method registration outside a package errors when unresolved", {
  foo <- new_generic("foo", "x")
  expect_snapshot(error = TRUE, {
    register_method(
      foo,
      new_external_class("not_loaded_pkg", "X"),
      function(x) "x",
      package = NULL
    )
  })
})

test_that("method registration with resolved external class works", {
  foo <- new_generic("foo", "x")
  # S7 is loaded, so this can resolve immediately
  method(foo, new_external_class("S7", "S7_object")) <- function(x) "s7"
  expect_equal(foo(S7_object()), "s7")
})

test_that("method_deps() collects the generic and external classes", {
  gen <- new_external_generic("foo", "bar", "x")
  sig <- list(
    new_external_class("baz", "X"),
    class_character,
    new_external_class("qux", "Y", version = "1.0")
  )
  deps <- method_deps(gen, sig)
  expect_equal(vcapply(deps, `[[`, "package"), c("foo", "baz", "qux"))
  expect_equal(deps[[3]]$version, "1.0")
})

test_that("dep_available() respects loaded + version", {
  # S7 is loaded, so this dep is available
  expect_true(dep_available(new_external_generic("S7", "S7_inherits", "x")))
  # version too high → not available
  expect_false(dep_available(
    new_external_generic("S7", "S7_inherits", "x", version = "999.0")
  ))
  # unloaded package → not available
  expect_false(dep_available(new_external_class("not_a_package", "X")))
})
