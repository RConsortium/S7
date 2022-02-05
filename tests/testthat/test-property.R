describe("prop", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(prop(x, "start"), 1)
    expect_equal(prop(x, "end"), 10)
  })
  it("evalutes dynamic properties", {
    x <- range(1, 10)
    expect_equal(prop(x, "length"), 9)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_snapshot_error(prop(x, "st"))
  })
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(r7_data(x), as_class("character")("hi"))
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text(val)
    expect_equal(r7_data(x), as_class("character")(val))
  })
  it("lets you set .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text("foo")
    r7_data(x) <- "bar"
    expect_equal(r7_data(x), as_class("character")("bar"))
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

describe("property access", {
  it("access en masse", {
    foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"))
    x <- foo(x = 1, y = 2)
    expect_equal(prop_names(x), c("x", "y"))
    expect_equal(props(x), list(x = 1, y = 2))
    expect_true(prop_exists(x, "x"))
    expect_true(prop_exists(x, "y"))
    expect_false(prop_exists(x, "z"))
  })

  it("can access dynamic properties", {
    foo <- new_class("foo", properties = list(
      new_property("x", getter = function(x) 10),
      new_property("y")
    ))
    x <- foo(y = 2)
    expect_equal(props(x), list(x = 10, y = 2))
  })

  it("can with property-less object", {
    x <- new_class("x")()
    expect_equal(prop_names(x), character())
    expect_equal(props(x), list())
    expect_equal(prop_exists(x, "y"), FALSE)
  })
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

  x <- foo(bar = "foo")
  expect_equal(x@bar, "foo-bar")
})

test_that("properties can be base, S3, S4, R7, or R7 union", {
  class_r7 <- new_class("class_r7")
  class_s4 <- methods::setClass("class_s4", slots = c(x = "numeric"))

  my_class <- new_class("my_class",
    properties = list(
      anything = NULL,
      base = "integer",
      s3 = s3_class("factor", factor),
      s4 = class_s4,
      r7 = class_r7,
      r7_union = new_union("integer", "logical")
    )
  )
  expect_snapshot(my_class)
  my_obj <- my_class(
    anything = TRUE,
    base = 1L,
    s3 = factor(),
    s4 = class_s4(x = 1),
    r7 = class_r7(),
    r7_union = 1L
  )

  # First check that we can set with out error
  expect_error(my_obj@base <- 2L, NA)
  expect_error(my_obj@s3 <- factor("x"), NA)
  expect_error(my_obj@s4 <- class_s4(x = 2), NA)
  expect_error(my_obj@r7 <- class_r7(), NA)
  expect_error(my_obj@r7_union <- 2L, NA)
  expect_error(my_obj@r7_union <- TRUE, NA)

  # Then capture the error messages for human inspection
  expect_snapshot(error = TRUE, {
    my_obj@base <- "x"
    my_obj@s3 <- "x"
    my_obj@s4 <- "x"
    my_obj@r7 <- "x"
    my_obj@r7_union <- "x"
  })
})
