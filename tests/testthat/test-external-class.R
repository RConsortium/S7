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

test_that("resolve_external_class_opt() resolves a loaded class", {
  # S7 is itself loaded, S7_object exists in it
  ec <- new_external_class("S7", "S7_object")
  resolved <- resolve_external_class_opt(ec)
  expect_true(is_class(resolved))
  expect_equal(resolved@name, "S7_object")
})

test_that("resolve_external_class_opt() returns NULL when unavailable", {
  expect_null(resolve_external_class_opt(new_external_class("not_a_pkg", "X")))
  expect_null(resolve_external_class_opt(new_external_class(
    "S7",
    "not_a_class"
  )))
})

test_that("resolve_external_class_req() errors per failure mode", {
  local_mocked_bindings(getNamespaceVersion = function(package) "1.0.0")
  expect_snapshot(error = TRUE, {
    resolve_external_class_req(new_external_class("not_a_pkg", "X"))
    resolve_external_class_req(new_external_class("S7", "S7_object", "2.0.0"))
    resolve_external_class_req(new_external_class("S7", "not_a_class"))
  })
})

test_that("external class can be used as a union arm", {
  ec <- new_external_class("foo", "Bar")
  u <- NULL | ec
  expect_s3_class(u, "S7_union")
  expect_length(u$classes, 2)
})

test_that("external class works as a property type for self-reference", {
  Tree := new_class(
    package = "mypkg",
    properties = list(
      label = class_character,
      child = NULL | new_external_class("mypkg", "Tree")
    )
  )

  leaf <- Tree(label = "leaf")
  expect_null(leaf@child)

  root <- Tree(label = "root", child = leaf)
  expect_equal(root@child@label, "leaf")

  # type checking still rejects wrong types
  expect_snapshot(error = TRUE, Tree(label = "bad", child = 1))
})

test_that("external class works for mutually recursive classes", {
  ClassOne := new_class(
    package = "mypkg",
    properties = list(x = NULL | new_external_class("mypkg", "ClassTwo"))
  )
  ClassTwo := new_class(
    package = "mypkg",
    properties = list(y = NULL | new_external_class("mypkg", "ClassOne"))
  )

  obj <- ClassOne(x = ClassTwo(y = ClassOne()))
  expect_s3_class(obj@x, "mypkg::ClassTwo")
  expect_s3_class(obj@x@y, "mypkg::ClassOne")
})

test_that("class_inherits() works for external class", {
  Tree := new_class(
    package = "mypkg",
    properties = list(child = NULL | new_external_class("mypkg", "Tree"))
  )
  ec <- new_external_class("mypkg", "Tree")
  expect_true(class_inherits(Tree(), ec))
  expect_false(class_inherits(1, ec))
  expect_false(class_inherits(NULL, ec))
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
