describe("S7_data", {
  text <- new_class("text", class_character)
  it("retrieves .data", {
    x <- text("hi")
    expect_equal(S7_data(x), "hi")
  })
  it("strips properties", {
    text <- new_class("text", class_character, properties = list(x = class_integer))
    x <- text("hi", x = 10L)
    expect_equal(attributes(S7_data(x)), NULL)
  })
  it("preserves non-property attributes when retrieving .data", {
    val <- c(foo = "hi", bar = "ho")
    expect_equal(names(S7_data(text(val))), names(val))
  })
  it("lets you set data", {
    val <- c(foo = "hi", bar = "ho")
    x <- text("foo")
    S7_data(x) <- "bar"
    expect_equal(S7_data(x), "bar")
  })
})


describe("S7_data<-", {

  it("uses updated 'names'", {
    # local_methods(`$<-`, `[[<-`) # no support for unregistering S3 generic methods

    write_once_list <- new_class("write_once_list", class_list,
      constructor = function(...) new_object(list(...))
    )

    method(`$<-`, write_once_list) <-
    method(`[[<-`, write_once_list) <- function(x, name, value) {
      .x <- S7_data(x)
      stopifnot(is_string(name))
      if (hasName(.x, name))
        stop("entry exists: ", name)
      .x[[name]] <- value
      S7_data(x) <- .x
      x
    }
    w <- write_once_list(x = 3, y = 4)
    w$bar <- 1
    expect_equal(names(w), c("x", "y", "bar"))
    expect_error(w$bar <- 2, "entry exists:")

  })

})
