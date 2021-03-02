describe("property", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(property(x, "start"), 1)
    expect_equal(property(x, "end"), 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_error(property(x, "st"), "`range` objects do not have a `st` property")
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
})

describe("@", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_error(x@st, "`range` objects do not have a `st` property")
  })
  # TODO: add tests to verify fallback for non-R7 objects
})

describe("@<-", {
  it("sets the property", {
    x <- range(1, 10)
    expect_equal(x@start, 1)
    x@start <- 2
    expect_equal(x@start, 2)
  })
})

test_that("properties can be accessor functions", {
  x <- range(1, 10)
  expect_equal(x@length, 10 - 1)
})
