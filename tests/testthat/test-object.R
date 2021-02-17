describe("object_new", {
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

  it("can instantiate a new with properties", {
    x <- range(start = 1, end = 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
  })

  it("checks new objects for validity", {
    expect_error(range(start = 10, end = 1), "`end` must be greater than or equal to `start`")
  })
})
