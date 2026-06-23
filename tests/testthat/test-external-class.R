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

test_that("resolve_external_class_req() errors per failure mode", {
  local_package("too.old", version = "1.0.0")

  expect_snapshot(error = TRUE, {
    resolve_external_class_req(new_external_class("not_a_pkg", "X"))
    resolve_external_class_req(new_external_class("too.old", "X", "2.0.0"))
  })
})

test_that("external class resolution errors if class binding contract is violated", {
  local_package("dep")
  Bar := new_external_class(package = "dep")
  expect_snapshot(find_external_class(Bar), error = TRUE)
})

test_that("external class can be used as a union arm", {
  ec <- new_external_class("foo", "Bar")
  u <- NULL | ec
  expect_s3_class(u, "S7_union")
  expect_length(u$classes, 2)
})

test_that("S7_inherits() matches loaded union arms around unloaded external classes", {
  Foo := new_class(package = NULL)
  Missing <- new_external_class(package = "S7testthatmissing", name = "Bar")

  expect_true(S7_inherits(Foo(), Foo | Missing))
  expect_true(S7_inherits(Foo(), Missing | Foo))
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

test_that("external class property validation reports validator errors", {
  dep := local_package({
    Ext := new_class(
      properties = list(x = class_integer),
      validator = function(self) {
        if (self@x < 0L) {
          "x must be non-negative"
        }
      }
    )
  })
  Holder := new_class(
    properties = list(
      child = new_property(
        class = new_external_class("dep", "Ext"),
        default = quote(dep$Ext(x = 0L))
      )
    )
  )

  valid <- Holder(child = dep$Ext(x = 1L))
  expect_s3_class(valid@child, "dep::Ext")

  invalid <- valid_implicitly(dep$Ext(x = 1L), function(self) {
    self@x <- -1L
    self
  })

  expect_snapshot(Holder(child = invalid), error = TRUE)
})
