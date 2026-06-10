describe("super()", {
  it("overrides dispatch, matching inherited behaviour", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)
    foo3 <- new_class("foo3", foo2)

    bar <- new_generic("bar", "x")
    method(bar, foo1) <- function(x) 1
    method(bar, foo3) <- function(x) 3

    expect_equal(bar(super(foo3(), to = foo2)), 1)
    expect_equal(bar(super(foo3(), to = foo1)), 1)
  })

  it("only affects one dispatch", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    bar1 <- new_generic("bar1", "x")
    method(bar1, foo1) <- function(x) 1
    method(bar1, foo2) <- function(x) 2

    bar2 <- new_generic("bar2", "x")
    method(bar2, foo1) <- function(x) c(1, bar1(x))
    method(bar2, foo2) <- function(x) c(2, bar1(x))

    expect_equal(bar2(super(foo2(), to = foo1)), c(1, 2))
    expect_equal(bar2(convert(foo2(), to = foo1)), c(1, 1))
  })

  it("checks to", {
    expect_snapshot(error = TRUE, {
      foo <- new_class("foo", package = NULL)
      super(foo(), class_character)
      super(foo(), class_numeric)
      super(foo(), NULL)
    })
  })

  it("works with S3 objects", {
    my_int <- structure(10L, class = c("MyInt", "integer"))

    gen <- new_generic("gen", "x")
    method(gen, new_S3_class("MyInt")) <- function(x) "MyInt"
    method(gen, class_integer) <- function(x) "integer"

    expect_equal(gen(my_int), "MyInt")
    expect_equal(gen(super(my_int, to = class_integer)), "integer")
  })

  it("works with abstract S3 classes (#686)", {
    gen <- new_generic("gen", "x")
    method(gen, class_POSIXct) <- function(x) "POSIXct"
    method(gen, class_POSIXt) <- function(x) "POSIXt"

    x <- .POSIXct(1)
    expect_equal(gen(x), "POSIXct")
    expect_equal(gen(super(x, to = class_POSIXt)), "POSIXt")
  })

  it("works with S4 objects", {
    methods::setClass("Foo1", representation(x = "numeric"))
    methods::setClass("Foo2", contains = "Foo1")
    on.exit(S4_remove_classes(c("Foo1", "Foo2")))
    obj <- methods::new("Foo2", x = 5)

    gen <- new_generic("gen", "x")
    method(gen, methods::getClass("Foo1")) <- function(x) "parent"
    method(gen, methods::getClass("Foo2")) <- function(x) "child"

    expect_equal(gen(obj), "child")
    expect_equal(gen(super(obj, to = methods::getClass("Foo1"))), "parent")
  })

  it("displays nicely", {
    foo1 <- new_class("foo1", package = NULL)
    foo2 <- new_class("foo2", foo1, package = NULL)

    expect_snapshot({
      f1 <- super(foo2(), foo1)
      f1
      str(list(f1))
    })
  })
})
