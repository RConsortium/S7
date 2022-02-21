describe("R7_data", {
  text <- new_class("text", parent = "character")
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(R7_data(x), "hi")
  })
  it("strips properties", {
    text <- new_class("text", parent = "character", properties = list(x = "integer"))
    x <- text("hi", x = 10L)
    expect_equal(attributes(R7_data(x)), NULL)
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text(val)
    expect_equal(R7_data(x), val)
  })
  it("lets you set data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text("foo")
    R7_data(x) <- "bar"
    expect_equal(R7_data(x), "bar")
  })
})
