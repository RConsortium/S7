test_that("can register convert methods", {
  local_methods(convert)
  converttest <- new_class("converttest", package = NULL)
  method(convert, list(converttest, class_character)) <- function(
    from,
    to,
    ...
  ) {
    "c"
  }
  method(convert, list(converttest, class_integer)) <- function(from, to, ...) {
    "i"
  }

  obj <- converttest()
  expect_equal(convert(obj, to = class_character), "c")
  expect_equal(convert(obj, to = class_integer), "i")

  # Errors if none found
  expect_snapshot(
    convert(obj, to = class_double),
    error = TRUE,
    # for < 4.4.0
    transform = \(x) gsub("'S4'", "'object'", x)
  )
})

test_that("doesn't convert to subclass", {
  local_methods(convert)
  converttest1 <- new_class("converttest1")
  converttest2 <- new_class("converttest2", converttest1)

  method(convert, list(class_integer, converttest1)) <- function(
    from,
    to,
    ...
  ) {
    "i"
  }
  expect_error(convert(class_integer, to = converttest2), "Can't find method")
})

describe("fallback convert", {
  local_methods(convert)

  it("can convert to own class", {
    foo1 <- new_class("foo1", package = NULL)
    foo2 <- new_class("foo2", foo1, package = NULL)

    obj <- convert(foo2(), to = foo2)
    expect_equal(class(obj), c("foo2", "foo1", "S7_object"))
    expect_equal(S7_class(obj), foo2)
  })

  it("can convert to super class", {
    foo1 <- new_class(
      "foo1",
      properties = list(x = class_double),
      package = NULL
    )
    foo2 <- new_class(
      "foo2",
      foo1,
      properties = list(y = class_double),
      package = NULL
    )

    obj <- convert(foo2(1, 2), to = foo1)
    expect_equal(class(obj), c("foo1", "S7_object"))
    expect_equal(S7_class(obj), foo1)
    expect_equal(props(obj), list(x = 1))
    expect_equal(attr(obj, "y"), NULL)
  })

  it("can convert to subclass", {
    Foo <- new_class("Foo", properties = list(x = class_numeric))
    Bar <- new_class("Bar", Foo, properties = list(y = class_numeric))

    foo <- Foo(x = 1)

    # Basic conversion
    bar <- convert(foo, Bar)
    expect_s3_class(bar, c("Bar", "Foo", "S7_object"))
    expect_equal(S7_class(bar), Bar)
    expect_equal(bar@x, 1)
    expect_equal(bar@y, numeric(0))

    # Overriding existing property
    bar <- convert(foo, Bar, x = 2)
    expect_equal(bar@x, 2)

    # Setting new property
    bar <- convert(foo, Bar, y = 2)
    expect_equal(bar@x, 1)
    expect_equal(bar@y, 2)

    # Setting both properties
    bar <- convert(foo, Bar, y = 2, x = 3)
    expect_equal(bar@x, 3)
    expect_equal(bar@y, 2)

    # Error on converting to unrelated class
    Unrelated <- new_class("Unrelated", properties = list(z = class_character))
    expect_error(convert(foo, Unrelated), "Can't find method")
  })

  it("can convert to S3 class", {
    factor2 <- new_class(
      "factor2",
      class_factor,
      properties = list(x = class_double)
    )
    obj <- convert(factor2(1, "x", x = 1), to = class_factor)
    expect_equal(class(obj), "factor")
    expect_false(S7_inherits(obj))
    expect_equal(attr(obj, "x"), NULL)
  })

  it("can convert to base type", {
    character2 <- new_class(
      "character2",
      parent = class_character,
      properties = list(x = class_double)
    )
    obj <- convert(character2("x", x = 1), to = class_character)
    expect_equal(attr(obj, "class"), NULL)
    expect_false(S7_inherits(obj))
    expect_equal(attr(obj, "x"), NULL)
  })

  it("can convert_up() an S4-derived S7 object to an S4 object ", {
    on.exit(S4_remove_classes(c("ParentS4", "ChildS7")))
    setClass("ParentS4", slots = list(x = "numeric"))

    ChildS7 <- new_class(
      "ChildS7",
      parent = getClass("ParentS4"),
      properties = list(y = class_character),
      package = NULL
    )

    child <- ChildS7(x = 10, y = "a")
    parent <- convert(child, to = getClass("ParentS4"))

    expect_true(isS4(parent))
    expect_equal(class(parent)[[1]], "ParentS4")
    expect_equal(methods::slot(parent, "x"), 10)
  })

  it("can convert an S4-derived S7 object to an S4 object via methods::as", {
    on.exit(S4_remove_classes(c("ParentS4", "ChildS7", "UnrelatedS4")))
    setClass("ParentS4", slots = list(x = "numeric"))
    setClass("UnrelatedS4", slots = list(z = "character"))

    ChildS7 <- new_class(
      "ChildS7",
      parent = getClass("ParentS4"),
      properties = list(y = class_character),
      package = NULL
    )

    setAs("ChildS7", "UnrelatedS4", function(from) {
      new("UnrelatedS4", z = as.character(methods::slot(from, "x")))
    })

    child <- ChildS7(x = 42, y = "a")
    res <- convert(child, to = getClass("UnrelatedS4"))

    expect_true(isS4(res))
    expect_equal(class(res)[[1]], "UnrelatedS4")
    expect_equal(methods::slot(res, "z"), "42")
  })
})

test_that("is_down_cast() is TRUE only when `to` descends from `from` (#509)", {
  Base <- new_class("Base", package = NULL)
  A <- new_class(
    "A",
    Base,
    package = NULL,
    properties = list(x = class_numeric)
  )
  B <- new_class("B", Base, package = NULL)
  B_child <- new_class("B_child", B, package = NULL)

  # `to` is a descendant of `from`'s class
  expect_equal(is_down_cast(Base(), A), TRUE)
  expect_equal(is_down_cast(B(), B_child), TRUE)

  # base/S3 `from` to an S7 class that extends it
  my_logical <- new_class("my_logical", class_logical, package = NULL)
  my_factor <- new_class("my_factor", class_factor, package = NULL)
  expect_equal(is_down_cast(TRUE, my_logical), TRUE)
  expect_equal(is_down_cast(factor("a"), my_factor), TRUE)

  # Siblings share an ancestor but neither descends from the other
  expect_equal(is_down_cast(B(), A), FALSE)
  expect_equal(is_down_cast(B_child(), A), FALSE)
})

test_that("is_down_cast() requires `from`'s classes to be contiguous and ordered", {
  # All of `from`'s classes are in `to`, but not contiguously
  gappy <- structure(list(), class = c("a", "b"))
  expect_equal(is_down_cast(gappy, new_S3_class(c("a", "x", "b"))), FALSE)

  # All of `from`'s classes are in `to`, but in the wrong order
  reversed <- structure(list(), class = c("b", "a"))
  expect_equal(is_down_cast(reversed, new_S3_class(c("a", "b"))), FALSE)

  # A genuine contiguous, ordered run succeeds
  ok <- structure(list(), class = c("b", "a"))
  expect_equal(is_down_cast(ok, new_S3_class(c("c", "b", "a"))), TRUE)
})

test_that("convert() is idempotent when `from` is an instance of `to` (#429)", {
  local_methods(convert)
  Foo <- new_class("Foo", package = NULL, properties = list(x = class_numeric))

  # A registered method must not override the idempotent behaviour
  method(convert, list(Foo, Foo)) <- function(from, to, ...) Foo(x = -1)

  foo <- Foo(x = 1)
  expect_identical(convert(foo, Foo), foo)
})

test_that("convert() upcasting ignores inherited downcasting methods (#429)", {
  local_methods(convert)
  A <- new_class("A", package = NULL, properties = list(x = class_numeric))
  B <- new_class("B", A, package = NULL, properties = list(y = class_numeric))
  C <- new_class("C", B, package = NULL, properties = list(z = class_numeric))

  # A method to downcast a superclass `A` to `B`
  method(convert, list(A, B)) <- function(from, to, ...) B(y = -1)

  # Converting a `C` (which is already a `B`) to `B` should upcast, dropping
  # `z`, rather than dispatch to the inherited `A` -> `B` downcasting method.
  b <- convert(C(x = 1, y = 2, z = 3), to = B)
  expect_equal(S7_class(b), B)
  expect_equal(b@x, 1)
  expect_equal(b@y, 2)

  # A more specific upcasting method is still used
  method(convert, list(C, B)) <- function(from, to, ...) B(y = 99)
  expect_equal(convert(C(x = 1, y = 2, z = 3), to = B)@y, 99)
})

test_that("convert() errors when upcasting to an abstract class (#680)", {
  Foo <- new_class("Foo", abstract = TRUE, package = NULL)
  Bar <- new_class("Bar", Foo, package = NULL)
  expect_snapshot(convert(Bar(), Foo), error = TRUE)
})

test_that("convert() falls back to as.*() for base type targets (#472)", {
  expect_identical(convert(1.5, class_character), "1.5")
  expect_identical(convert(c("1", "2"), class_integer), c(1L, 2L))
  expect_identical(convert(0:1, class_logical), c(FALSE, TRUE))
  expect_identical(convert(1:2, class_double), c(1, 2))
  expect_identical(convert(c("a", "b"), class_list), list("a", "b"))
  expect_identical(convert("x", class_name), as.name("x"))
})

test_that("base type fallback sits below user methods and inheritance", {
  local_methods(convert)

  # A registered method wins over the as.*() fallback
  Txt <- new_class("Txt", class_character, package = NULL)
  method(convert, list(Txt, class_character)) <- function(from, to, ...) {
    "custom"
  }
  expect_equal(convert(Txt("hi"), class_character), "custom")

  # Upcasting to a base type strips the S7 wrapper rather than calling as.*()
  method(convert, list(Txt, class_character)) <- NULL
  obj <- convert(Txt("hi"), class_character)
  expect_false(S7_inherits(obj))
  expect_null(attributes(obj))
  expect_identical(obj, "hi")
})
