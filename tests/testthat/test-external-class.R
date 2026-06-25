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

test_that("resolve_external_class_req() resolves a package's own unexported class", {
  pkg <- local_package("ownpkg", {
    Hidden := new_class()
  })
  # remove from exports
  rm("Hidden", envir = pkg[[".__NAMESPACE__."]]$exports)
  Hidden := new_external_class("ownpkg")

  # fails from the outside
  expect_error(
    resolve_external_class_req(Hidden),
    class = "S7_error_external_class_unresolved"
  )
  # but a package can refer to its own unexported classes
  expect_equal(
    resolve_external_class_req(Hidden, package = "ownpkg"),
    pkg$Hidden
  )
})

test_that("resolve_external_class_req() errors per failure mode", {
  local_package("too.old", version = "1.0.0")

  expect_snapshot(error = TRUE, {
    resolve_external_class_req(new_external_class("not_a_pkg", "X"))
    resolve_external_class_req(new_external_class("too.old", "X", "2.0.0"))
    resolve_external_class_req(new_external_class("too.old", "X"))
  })
})

test_that("external class can be used as a union arm", {
  ec := new_external_class("foo")
  u <- NULL | ec
  expect_s3_class(u, "S7_union")
  expect_length(u$classes, 2)
})

test_that("S7_inherits() matches loaded union arms around unloaded external classes", {
  Foo := new_class()
  Bar := new_external_class(package = "S7testthatmissing")

  expect_true(S7_inherits(Foo(), Foo | Bar))
  expect_true(S7_inherits(Foo(), Bar | Foo))
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
  expect_snapshot(Tree(label = "bad", child = 1), error = TRUE)
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

test_that("an external class can be used as a parent (#317)", {
  dep := local_package({
    Animal := new_class(
      properties = list(name = class_character, legs = class_integer)
    )
  })

  Animal := new_external_class(package = "dep")
  Dog := new_class(
    parent = Animal,
    properties = list(breed = class_character)
  )
  # parent properties are captured statically; construction forwards `...`
  expect_named(Dog@properties, c("name", "legs", "breed"))
  expect_named(formals(Dog), c("...", "breed"))

  d <- Dog(name = "Rex", legs = 4L, breed = "lab")
  expect_equal(d@name, "Rex")
  expect_equal(d@legs, 4L)
  expect_equal(d@breed, "lab")
  expect_s3_class(d, "dep::Animal")
  expect_true(S7_inherits(d, Animal))
})

test_that("subclass constructs against the parent's run-time definition (#317)", {
  dep := local_package({
    Animal := new_class(
      properties = list(legs = new_property(class_integer, default = 4L))
    )
  })
  Animal := new_external_class(package = "dep")

  Dog := new_class(
    parent = Animal,
    properties = list(breed = class_character)
  )
  expect_equal(Dog()@legs, 4L)

  # The parent package changes the default and is reloaded; the subclass is
  # *not* rebuilt, yet its constructor picks up the new definition.
  dep$Animal <- new_class(
    name = "Animal",
    package = "dep",
    properties = list(legs = new_property(class_integer, default = 6L))
  )
  expect_equal(Dog()@legs, 6L)
  # explicit values are still forwarded to the parent
  expect_equal(Dog(legs = 2L, breed = "x")@legs, 2L)
})

test_that("subclassing errors when the external parent can't be resolved", {
  dep := local_package(version = "1.0.0", {
    Animal := new_class()
  })
  Animal := new_external_class(package = "dep", version = "2.0.0")
  expect_snapshot(new_class(name = "Dog", parent = Animal), error = TRUE)
})
