describe("object_new", {
  it("can instantiate a new object with properties", {
    x <- range(start = 1, end = 10)
    expect_equal(x@start, 1)
    expect_equal(x@end, 10)
  })

  it("checks new objects for validity", {
    expect_error(range(start = 10, end = 1), "`end` must be greater than or equal to `start`")
  })


  it("can instantiate a new object that inherits from a basic type", {
    x <- text()
    expect_equal(x@.data, character())

    y <- text("foo")
    expect_equal(y@.data, "foo")
  })

  it("errors if given an invalid property", {
    expect_error(
      range(1, "foo"),
      "must be of class"
    )
  })
})
