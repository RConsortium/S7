describe("R7_data", {
  text <- new_class("text", class_character)
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(R7_data(x), "hi")
  })
  it("strips properties", {
    text <- new_class("text", class_character, properties = list(x = class_integer))
    x <- text("hi", x = 10L)
    expect_equal(attributes(R7_data(x)), NULL)
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    expect_equal(names(R7_data(text(val))), names(val))
  })
  it("lets you set data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text("foo")
    R7_data(x) <- "bar"
    expect_equal(R7_data(x), "bar")
  })
})
