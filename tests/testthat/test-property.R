describe("property", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(property(x, "start"), 1)
    expect_equal(property(x, "end"), 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_snapshot_error(property(x, "st"))
  })
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(x@.data, "hi")
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text(val)
    expect_equal(x@.data, val)
  })
  it("lets you set .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text("foo")
    x@.data <- "bar"
    expect_equal(x@.data, "bar")
  })
})

describe("property<-", {
  it("sets the property", {
    x <- range(1, 10)
    expect_equal(property(x, "start"), 1)
    property(x, "start") <- 2
    expect_equal(property(x, "start"), 2)
  })
  it("errors if the value does not match the correct class", {
    x <- range(1, 10)
    expect_error(
      property(x, "start") <- "foo",
      "must be of class"
    )
  })
  it("does not run the check or validation functions if check = FALSE", {
    x <- range(1, 10)
    property(x, "end", check = FALSE) <- "foo"
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
    x <- "foo"
    expect_error(x@blah, "trying to get slot")
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

test_that("properties can be getter functions", {
  x <- range(1, 10)
  expect_equal(x@length, 10 - 1)
})

test_that("properties can be setter functions", {
  x <- range(1, 10)
  x@length <- 5
  expect_equal(x@length, 5)
})

