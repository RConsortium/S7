test_that("can register convert methods", {
  local_methods(convert)
  converttest <- new_class("converttest")
  method(convert, list(converttest, class_character)) <- function(from, to, ...) "c"
  method(convert, list(converttest, class_integer)) <- function(from, to, ...) "i"

  obj <- converttest()
  expect_equal(convert(obj, to = class_character), "c")
  expect_equal(convert(obj, to = class_integer), "i")

  # Errors if none found
  expect_snapshot(convert(obj, to = class_double), error = TRUE)
})

test_that("doesn't convert to subclass", {
  local_methods(convert)
  converttest1 <- new_class("converttest1")
  converttest2 <- new_class("converttest2", converttest1)

  method(convert, list(class_integer, converttest1)) <- function(from, to, ...) "i"
  expect_error(convert(class_integer, to = converttest2), "Can't find method")
})

describe("fallback convert", {
  local_methods(convert)

  it("can convert to own class", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    obj <- convert(foo2(), to = foo2)
    expect_equal(class(obj), c("foo2", "foo1", "S7_object"))
    expect_equal(S7_class(obj), foo2)
  })

  it("can convert to super class", {
    foo1 <- new_class("foo1", properties = list(x = class_double))
    foo2 <- new_class("foo2", foo1, properties = list(y = class_double))

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
    factor2 <- new_class("factor2", class_factor, properties = list(x = class_double))
    obj <- convert(factor2(1, "x", x = 1), to = class_factor)
    expect_equal(class(obj), "factor")
    expect_equal(S7_class(obj), NULL)
    expect_equal(attr(obj, "x"), NULL)
  })

  it("can convert to base type", {
    character2 <- new_class("character2",
      parent = class_character,
      properties = list(x = class_double)
    )
    obj <- convert(character2("x", x = 1), to = class_character)
    expect_equal(attr(obj, "class"), NULL)
    expect_equal(S7_class(obj), NULL)
    expect_equal(attr(obj, "x"), NULL)
  })
})
