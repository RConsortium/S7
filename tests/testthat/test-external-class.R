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
  ec <- new_external_class(package = "foo", name = "Bar")

  expect_identical(as_class(ec), ec)
  expect_equal(S7_class_desc(ec), "<foo::Bar>")
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

test_that("external class property validation reports validator errors", {
  dep := local_package(
    Ext := new_class(
      properties = list(x = class_integer),
      validator = function(self) {
        if (self@x < 0L) {
          "x must be non-negative"
        }
      }
    )
  )
  Holder := new_class(
    properties = list(
      child = new_property(
        class = new_external_class("dep", "Ext"),
        default = quote(dep$Ext(x = 0L))
      )
    )
  )

  invalid <- valid_implicitly(dep$Ext(x = 1L), function(self) {
    self@x <- -1L
    self
  })

  expect_snapshot(Holder(child = invalid), error = TRUE)
})

test_that("external class property validation uses resolved dispatch", {
  Holder := new_class(
    properties = list(x = new_external_class("S7", "S7_object"))
  )

  expect_s3_class(Holder(x = S7_object())@x, "S7_object")
})

test_that("versioned external class checks package version", {
  versioned_pkg := local_package(
    Foo := new_class()
  )
  Foo <- new_external_class(
    package = "versioned_pkg",
    name = "Foo",
    version = "999.0"
  )

  expect_snapshot(error = TRUE, S7_inherits(versioned_pkg$Foo(), Foo))
})
