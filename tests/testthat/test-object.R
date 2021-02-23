describe("object_new", {
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

  it("can instantiate a new object with properties", {
    x <- range(start = 1, end = 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
  })

  it("checks new objects for validity", {
    expect_error(range(start = 10, end = 1), "`end` must be greater than or equal to `start`")
  })


  it("can instantiate a new object that inherits from a basic type", {
    text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
    x <- text()
    expect_equal(x@.data, character())

    y <- text("foo")
    expect_equal(y@.data, "foo")
  })
})
