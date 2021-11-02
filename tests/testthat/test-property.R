describe("prop", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(prop(x, "start"), 1)
    expect_equal(prop(x, "end"), 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_snapshot_error(prop(x, "st"))
  })
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(x@.data, class_get("character")("hi"))
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text(val)
    expect_equal(x@.data, class_get("character")(val))
  })
  it("lets you set .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text("foo")
    x@.data <- "bar"
    expect_equal(x@.data, class_get("character")("bar"))
  })
})

describe("prop<-", {
  it("sets the property", {
    x <- range(1, 10)
    expect_equal(prop(x, "start"), 1)
    prop(x, "start") <- 2
    expect_equal(prop(x, "start"), 2)
  })
  it("errors if the value does not match the correct class", {
    x <- range(1, 10)
    expect_error(
      prop(x, "start") <- "foo",
      "must be of class"
    )
  })
  it("does not run the check or validation functions if check = FALSE", {
    x <- range(1, 10)
    prop(x, "end", check = FALSE) <- "foo"
    expect_equal(x@end, "foo")
  })
})

describe("@", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_snapshot_error(x@st)
  })
  it("falls back to `base::@` for non-R7 objects", {
    expect_snapshot(error = TRUE, {
      "foo"@blah
      NULL@blah
    })
  })
})

describe("@<-", {
  it("sets the property", {
    x <- range(1, 10)
    expect_equal(x@start, 1)
    x@start <- 2
    expect_equal(x@start, 2)
  })
  it("falls back to `base::@` for non-R7 objects", {
    x <- "foo"
    expect_error(x@blah <- "bar", "is not a slot in class")
  })
})

test_that("can access properties en masse", {
  foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"))
  x <- foo(x = 1, y = 2)
  expect_equal(prop_names(x), c("x", "y"))
  expect_equal(props(x), list(x = 1, y = 2))
  expect_true(prop_exists(x, "x"))
  expect_true(prop_exists(x, "y"))
  expect_false(prop_exists(x, "z"))
})

test_that("and works with property-less object", {
  x <- new_class("x")()
  expect_equal(prop_names(x), character())
  expect_equal(props(x), list())
  expect_equal(prop_exists(x, "y"), FALSE)
})

test_that("properties ignore attributes", {
  x <- new_class("x")()
  attr(x, "extra") <- 1

  expect_equal(prop_names(x), character())
  expect_equal(props(x), list())
  expect_false(prop_exists(x, "extra"))
})

test_that("properties can be getter functions", {
  x <- range(1, 10)
  expect_equal(x@length, 10 - 1)
})

test_that("properties can be setter functions", {
  x <- range(1, 10)
  x@length <- 5
  expect_equal(x@length, 5)
})

test_that("property setters can set themselves", {
  foo <- new_class("foo",
    properties = list(
      new_property(
        name = "bar",
        class = "character",
        setter = function(object, value) {
          object@bar <- paste0(value, "-bar")
          object
        }
      )
    )
  )

  x <- foo()

  x@bar <- "foo"

  expect_equal(x@bar, "foo-bar")
})
