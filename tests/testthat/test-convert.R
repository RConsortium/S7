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

  it("can convert to S4 class using methods::as", {
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
