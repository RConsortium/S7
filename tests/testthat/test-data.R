test_that("saved legacy mappings support data access and replacement", {
  skip_if_not_installed("ggplot2", "4.0.0")
  x <- readRDS(test_path("fixtures", "legacy-ggforce-mapping.rds"))
  data <- unclass(x)

  expect_identical(S7_inherits(x), TRUE)
  expect_invisible(check_is_S7(x))
  expect_identical(S7_data(x), data)

  y <- x
  S7_data(y) <- list(colour = "red")
  expect_identical(class(y), class(x))
  expect_identical(S7_data(y), list(colour = "red"))

  x["colour"] <- list(colour = "blue")
  expect_identical(x$colour, "blue")
  expect_identical(x$linewidth, data$linewidth)
  expect_identical(S7_inherits(x, ggplot2::class_mapping), TRUE)
})

test_that("saved legacy class metadata retains properties", {
  x <- readRDS(test_path("fixtures", "legacy-object.rds"))
  expect_identical(S7_inherits(x), TRUE)
  expect_identical(S7_class(x)@name, "Legacy")
  expect_identical(prop(x, "label"), "old")
  expect_identical(S7_data(x), list(x = 1L))

  prop(x, "label") <- "updated"
  S7_data(x) <- list(y = 2L)
  expect_identical(prop(x, "label"), "updated")
  expect_identical(S7_data(x), list(y = 2L))
})

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

test_that("S7_data preserves user S7_version attributes (#711)", {
  Text := new_class(parent = class_character)
  data <- structure("hi", S7_version = "user")
  x <- Text(.data = data)
  expect_identical(attr(x, "S7_version", exact = TRUE), "user")
  expect_identical(S7_data(x), data)

  replacement <- structure("bye", S7_version = "replacement")
  S7_data(x) <- replacement
  expect_identical(attr(x, "_S7_version", exact = TRUE), 1L)
  expect_identical(S7_data(x), replacement)
})

test_that("S7_data lets you set data", {
  text := new_class(class_character)
  x <- text("foo")
  S7_data(x) <- "bar"
  expect_equal(x, text("bar"))
})

test_that("S7_data preserves the object's representation version (#711)", {
  Text := new_class(parent = class_character)
  x <- Text(.data = "foo")
  S7_data(x) <- "bar"
  expect_identical(attr(x, "_S7_version", exact = TRUE), 1L)
  expect_identical(S7_data(x), "bar")

  attr(x, "_S7_version") <- NULL
  S7_data(x) <- Text(.data = "baz")
  expect_null(attr(x, "_S7_version", exact = TRUE))
  expect_identical(S7_data(x), "baz")
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

test_that("S7_data preserves S3 class from an external parent", {
  dep := local_package({
    MyDF := new_class(parent = class_data.frame)
  })
  MyDF := new_external_class(package = "dep")
  Sub := new_class(parent = MyDF)

  df <- data.frame(x = 1, y = 2)
  expect_equal(S7_data(Sub(df)), df)
})

test_that("S7_data does not add class when parent is a base type", {
  text := new_class(class_character)
  mychar := new_class(class_character)
  expect_null(attr(S7_data(mychar("x")), "class", exact = TRUE))
})
