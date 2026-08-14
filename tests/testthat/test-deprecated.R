test_that("deprecated_generic() warns then delegates to the replacement", {
  new_gen := new_generic("x")
  method(new_gen, class_double) <- function(x) mean(x)
  old_gen := deprecated_generic(new = new_gen, when = "1.1.0")

  expect_snapshot(out <- old_gen(c(1, 2, 3)))
  expect_equal(out, 2)
  expect_identical(formals(old_gen), formals(new_gen))
})

test_that("method registration on a deprecated generic targets the replacement", {
  new_gen := new_generic("x")
  old_gen := deprecated_generic(new = new_gen, when = "1.1.0")

  method(old_gen, class_character) <- function(x) toupper(x)
  expect_equal(new_gen("hi"), "HI")

  # method() and S7_methods() introspection unwrap too
  expect_equal(method(old_gen, class_character)(x = "hi"), "HI")
  expect_equal(S7_methods(old_gen)$generic, "new_gen")
})

test_that("deprecated_generic() without a replacement still dispatches", {
  old_gen := new_generic("x")
  old_gen := deprecated_generic(old = old_gen, when = "2.0.0")
  method(old_gen, class_character) <- function(x) toupper(x)

  expect_snapshot(out <- old_gen("hi"))
  expect_equal(out, "HI")
})

test_that("external generic registration resolves through a deprecated generic", {
  local_package("pkgA", {
    new_gen := new_generic("x")
    old_gen := deprecated_generic(new = new_gen, when = "1.1.0")
  })
  local_package("pkgB", {
    old_gen := new_external_generic("pkgA", dispatch_args = "x")
    method(old_gen, class_character) <- function(x) toupper(x)
  })

  expect_equal(asNamespace("pkgA")$new_gen("hi"), "HI")
})

test_that("deprecated_generic() validates its inputs", {
  new_gen := new_generic("x")
  expect_snapshot(error = TRUE, {
    deprecated_generic(1, new = new_gen, when = "1.0.0")
    deprecated_generic("old_gen", new = new_gen)
    deprecated_generic("old_gen", new = new_gen, when = "next year")
    deprecated_generic("old_gen", new = mean, when = "1.0.0")
    deprecated_generic("old_gen", when = "1.0.0")
    deprecated_generic("old_gen", new = new_gen, old = new_gen, when = "1.0.0")
    deprecated_generic("old_gen", old = mean, when = "1.0.0")
    deprecated_generic("old_gen", old = new_gen, when = "1.0.0")
    deprecated_generic(
      "old_gen",
      new = new_gen,
      when = "1.0.0",
      method = "warn"
    )
  })
})

test_that("deprecated_class() constructor warns then constructs the replacement", {
  Pet := new_class(properties = list(name = class_character))
  Dog := deprecated_class(new = Pet, when = "2.0.0")

  expect_snapshot(d <- Dog(name = "Fido"))
  expect_identical(S7_class(d), Pet)
  expect_identical(formals(Dog), formals(Pet))
})

test_that("deprecated class is silently treated as the replacement in class contexts", {
  Pet := new_class(properties = list(name = class_character))
  Dog := deprecated_class(new = Pet, when = "2.0.0")

  expect_identical(as_class(Dog), Pet)

  speak := new_generic("x")
  method(speak, Dog) <- function(x) "Woof!"
  expect_equal(speak(Pet(name = "Rex")), "Woof!")

  BigDog := new_class(parent = Dog)
  expect_identical(BigDog@parent, Pet)
  expect_no_warning(big <- BigDog(name = "Rex"))
  expect_equal(big@name, "Rex")
})

test_that("deprecated_class() without a replacement still constructs", {
  Cat := new_class(properties = list(lives = class_double))
  Cat := deprecated_class(old = Cat, when = "3.0.0")

  expect_snapshot(felix <- Cat(lives = 9))
  expect_equal(felix@lives, 9)
  expect_equal(S7_class(felix)@name, "Cat")
})

test_that("deprecated_class() validates its inputs", {
  Pet := new_class()
  expect_snapshot(error = TRUE, {
    deprecated_class(1, new = Pet, when = "1.0.0")
    deprecated_class("Old", new = Pet)
    deprecated_class("Old", new = 1, when = "1.0.0")
    deprecated_class("Old", when = "1.0.0")
    deprecated_class("Old", new = Pet, old = Pet, when = "1.0.0")
    deprecated_class("Old", old = 1, when = "1.0.0")
    deprecated_class("Old", old = Pet, when = "1.0.0")
  })
})

test_that("deprecated_property() with a replacement delegates and warns", {
  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property("count", new = "size", when = "1.5.0")
    )
  )

  b <- Basket(size = 3)
  expect_snapshot({
    print(b@count)
    b@count <- 5
  })
  expect_equal(b@size, 5)
})

test_that("deprecated_property() only warns at construction when actually used", {
  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property("count", new = "size", when = "1.5.0")
    )
  )

  expect_no_warning(Basket(size = 3))
  expect_snapshot(b <- Basket(count = 7))
  expect_equal(b@size, 7)
})

test_that("deprecated_property() without a replacement still stores data", {
  Hat := new_class(
    properties = list(
      deprecated_property("brim", when = "0.9.0", class = class_double)
    )
  )

  expect_no_warning(h <- Hat(brim = 2))
  expect_snapshot({
    print(h@brim)
    h@brim <- 3
  })
  expect_equal(attr(h, "brim"), 3)
})

test_that("deprecated_property() validates its inputs", {
  expect_snapshot(error = TRUE, {
    deprecated_property(1, when = "1.0.0")
    deprecated_property("count", new = 1, when = "1.0.0")
    deprecated_property("count", new = "size")
    deprecated_property("count", new = "size", when = "next year")
    deprecated_property(
      "count",
      new = "size",
      when = "1.0.0",
      method = "warn"
    )
  })
})

test_that("deprecation warnings mention the package", {
  pkg <- local_package("pkgA", {
    new_gen := new_generic("x")
    method(new_gen, class_double) <- function(x) x
    old_gen := deprecated_generic(new = new_gen, when = "1.1.0")
  })

  expect_snapshot(invisible(pkg$old_gen(1)))
})

test_that("method = 'lifecycle(warn)' signals with lifecycle", {
  skip_if_not_installed("lifecycle")

  pkg <- local_package("pkgA", {
    new_gen := new_generic("x")
    method(new_gen, class_double) <- function(x) x
    old_gen := deprecated_generic(
      new = new_gen,
      when = "1.1.0",
      method = "lifecycle(warn)"
    )
  })

  expect_snapshot(invisible(pkg$old_gen(1)))
})

test_that("method = 'lifecycle(stop)' errors", {
  skip_if_not_installed("lifecycle")

  new_gen := new_generic("x")
  method(new_gen, class_double) <- function(x) x
  old_gen := deprecated_generic(
    new = new_gen,
    when = "1.1.0",
    method = "lifecycle(stop)"
  )

  expect_snapshot(old_gen(1), error = TRUE)
})

test_that("deprecated_property() works with lifecycle", {
  skip_if_not_installed("lifecycle")

  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property(
        "count",
        new = "size",
        when = "1.5.0",
        method = "lifecycle(warn)"
      )
    )
  )
  b <- Basket(size = 3)

  expect_snapshot(invisible(b@count))
})

test_that("deprecated generics and classes print nicely", {
  new_gen := new_generic("x")
  old_gen := deprecated_generic(new = new_gen, when = "1.1.0")
  Pet := new_class()
  Dog := deprecated_class(new = Pet, when = "2.0.0")
  Cat := new_class()
  Cat := deprecated_class(old = Cat, when = "3.0.0")

  expect_snapshot({
    print(old_gen)
    print(Dog)
    print(Cat)
  })
})
