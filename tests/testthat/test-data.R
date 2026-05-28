describe("S7_data", {
  text <- new_class("text", class_character)
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(S7_data(x), "hi")
  })
  it("strips properties", {
    text <- new_class(
      "text",
      class_character,
      properties = list(x = class_integer)
    )
    x <- text("hi", x = 10L)
    expect_equal(attributes(S7_data(x)), NULL)
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    expect_equal(names(S7_data(text(val))), names(val))
  })
  it("lets you set data", {
    x <- text("foo")
    S7_data(x) <- "bar"
    expect_equal(x, text("bar"))
  })
  it("preserves names from the new data (#478)", {
    foo <- new_class("foo", class_list)
    x <- foo(list(a = 1, b = 2, c = 3))
    S7_data(x) <- list(b = 2, c = 3)
    expect_equal(x, foo(list(b = 2, c = 3)))

    x <- foo(list(a = 1, b = 2))
    S7_data(x) <- foo(list(a = 1, b = 2, c = 3))
    expect_equal(x, foo(list(a = 1, b = 2, c = 3)))
  })
  it("preserves S7 properties when setting data", {
    foo <- new_class(
      "foo",
      class_list,
      properties = list(extra = class_character)
    )
    x <- foo(list(a = 1), extra = "hi")
    S7_data(x) <- list(z = 99)
    expect_equal(x@extra, "hi")
    expect_equal(names(x), "z")
  })
  it("preserves S3 class from parent (#380)", {
    mydf <- new_class("mydf", class_data.frame)
    df <- data.frame(x = 1, y = 2)
    expect_equal(S7_data(mydf(df)), df)
  })
  it("preserves S3 class from grandparent", {
    mydf <- new_class("mydf", class_data.frame)
    mydf2 <- new_class("mydf2", mydf)
    df <- data.frame(x = 1, y = 2)
    expect_equal(S7_data(mydf2(df)), df)
  })
  it("does not add class when parent is a base type", {
    mychar <- new_class("mychar", class_character)
    expect_null(attr(S7_data(mychar("x")), "class", exact = TRUE))
  })
})
