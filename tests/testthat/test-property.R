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
})

describe("prop<-", {
  it("sets the property", {
    x <- range(1, 10)
    expect_equal(prop(x, "start"), 1)
    prop(x, "start") <- 2
    expect_equal(prop(x, "start"), 2)
  })
  it("errors if the property doesn't exist", {
    x <- range(1, 10)
    expect_snapshot(error = TRUE, x@foo <- 10)
  })
  it("errors if the value does not match the correct class", {
    x <- range(1, 10)
    expect_error(prop(x, "start") <- "foo", "must be")
  })
  it("does not run the check or validation functions if check = FALSE", {
    x <- range(1, 10)
    prop(x, "end", check = FALSE) <- "foo"
    expect_equal(x@end, "foo")
  })
})

describe("props<-", {
  it("validates after setting all properties", {
    x <- range(1, 2)
    props(x) <- list(start = 5, end = 10)
    expect_equal(x@start, 5)
    expect_equal(x@end, 10)
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

test_that("properties can be NULL", {
  foo <- new_class("foo", properties = list(x = NULL))
  x <- foo(x = NULL)
  expect_equal(x@x, NULL)
  x@x <- 1
  expect_equal(x@x, 1)
  x@x <- NULL
  expect_equal(x@x, NULL)
  expect_equal(prop_names(x), "x")
  expect_equal(props(x), list(x = NULL))
})

describe("new_property()", {
  it("validates name", {
    expect_snapshot(error = TRUE, {
      new_property(1)
      new_property("")
    })
  })

  it("validates default", {
    expect_snapshot(error = TRUE, {
      new_property("foo", class = "integer", default = "x")
    })
  })

  it("displays nicely", {
    x <- new_property("foo", "integer")
    expect_snapshot({
      print(x)
      str(list(x))
    })
  })
})

test_that("properties can be base, S3, S4, R7, or R7 union", {
  class_R7 <- new_class("class_R7")
  class_S4 <- methods::setClass("class_S4", slots = c(x = "numeric"))

  my_class <- new_class("my_class",
    properties = list(
      anything = NULL,
      base = "integer",
      S3 = new_S3_class("factor"),
      S4 = class_S4,
      R7 = class_R7,
      R7_union = new_union("integer", "logical")
    )
  )
  expect_snapshot(my_class)
  my_obj <- my_class(
    anything = TRUE,
    base = 1L,
    S3 = factor(),
    S4 = class_S4(x = 1),
    R7 = class_R7(),
    R7_union = 1L
  )

  # First check that we can set with out error
  expect_error(my_obj@base <- 2L, NA)
  expect_error(my_obj@S3 <- factor("x"), NA)
  expect_error(my_obj@S4 <- class_S4(x = 2), NA)
  expect_error(my_obj@R7 <- class_R7(), NA)
  expect_error(my_obj@R7_union <- 2L, NA)
  expect_error(my_obj@R7_union <- TRUE, NA)

  # Then capture the error messages for human inspection
  expect_snapshot(error = TRUE, {
    my_obj@base <- "x"
    my_obj@S3 <- "x"
    my_obj@S4 <- "x"
    my_obj@R7 <- "x"
    my_obj@R7_union <- "x"
  })
})

test_that("as_properties normalises properties", {
  expect_equal(as_properties(NULL), list())
  expect_equal(
    as_properties(list(new_property("y"))),
    list(y = new_property("y")
  ))
  expect_equal(
    as_properties(list(x = "numeric")),
    list(x = new_property("x", "numeric")
  ))
})

test_that("as_properties() gives useful error messages", {
  expect_snapshot(error = TRUE, {
    as_properties(1)
    as_properties(list(1))
    as_properties(list(x = 1))
    as_properties(list(x = "character", x = "character"))
  })
})
