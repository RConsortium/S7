range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end)
  },
  validator = function(x) {
    if (property(x, "end") < property(x, "start")) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = c(start = "numeric", end = "numeric")
)

describe("property", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(property(x, "start"), 1)
    expect_equal(property(x, "end"), 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_true(is.null(property(x, "st")))
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
    expect_true(is.null(x@st))
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
