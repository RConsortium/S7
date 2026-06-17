test_that("S7_data retrieves .data", {
  text := new_class(class_character)
  x <- text("hi")
  expect_equal(S7_data(x), "hi")
})

test_that("S7_data strips properties", {
  text := new_class(class_character)
  text := new_class(
    class_character,
    properties = list(x = class_integer)
  )
  x <- text("hi", x = 10L)
  expect_equal(attributes(S7_data(x)), NULL)
})

test_that("S7_data preserves non-property attributes when retrieving .data", {
  text := new_class(class_character)
  val <- c(foo = "hi", bar = "ho")
  expect_equal(names(S7_data(text(val))), names(val))
})

test_that("S7_data lets you set data", {
  text := new_class(class_character)
  x <- text("foo")
  S7_data(x) <- "bar"
  expect_equal(x, text("bar"))
})

test_that("S7_data preserves names from the new data (#478)", {
  text := new_class(class_character)
  foo := new_class(class_list)
  x <- foo(list(a = 1, b = 2, c = 3))
  S7_data(x) <- list(b = 2, c = 3)
  expect_equal(x, foo(list(b = 2, c = 3)))

  x <- foo(list(a = 1, b = 2))
  S7_data(x) <- foo(list(a = 1, b = 2, c = 3))
  expect_equal(x, foo(list(a = 1, b = 2, c = 3)))
})

test_that("S7_data preserves S7 properties when setting data", {
  text := new_class(class_character)
  foo := new_class(
    class_list,
    properties = list(extra = class_character)
  )
  x <- foo(list(a = 1), extra = "hi")
  S7_data(x) <- list(z = 99)
  expect_equal(x@extra, "hi")
  expect_equal(names(x), "z")
})

test_that("S7_data preserves S3 class from parent (#380)", {
  text := new_class(class_character)
  mydf := new_class(class_data.frame)
  df <- data.frame(x = 1, y = 2)
  expect_equal(S7_data(mydf(df)), df)
})

test_that("S7_data preserves S3 class from grandparent", {
  text := new_class(class_character)
  mydf := new_class(class_data.frame)
  mydf2 := new_class(mydf)
  df <- data.frame(x = 1, y = 2)
  expect_equal(S7_data(mydf2(df)), df)
})

test_that("S7_data does not add class when parent is a base type", {
  text := new_class(class_character)
  mychar := new_class(class_character)
  expect_null(attr(S7_data(mychar("x")), "class", exact = TRUE))
})
