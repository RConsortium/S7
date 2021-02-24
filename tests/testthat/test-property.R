range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end, length = accessor(function(x) x@end - x@start))
  },
  validator = function(x) {
    if (property(x, "end") < property(x, "start")) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = c(start = "numeric", end = "numeric", length = "accessor")
)

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
