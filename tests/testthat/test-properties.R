range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end)
  },
  validator = function(x) {
    if (prop(x, "end") < prop(x, "start")) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = c(start = "numeric", end = "numeric")
)

describe("prop", {
  it("retrieves the property", {
    x <- range(1, 10)
    expect_equal(prop(x, "start"), 1)
    expect_equal(prop(x, "end"), 10)
  })
  it("does not use partial matching", {
    x <- range(1, 10)
    expect_true(is.null(prop(x, "st")))
  })
})

describe("prop<-", {
  it("sets the property", {
    x <- range(1, 10)
    expect_equal(prop(x, "start"), 1)
    prop(x, "start") <- 2
    expect_equal(prop(x, "start"), 2)
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
