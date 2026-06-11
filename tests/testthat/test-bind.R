test_that(":= uses the variable name as `name`", {
  new_thing <- function(name, value = NULL) list(name = name, value = value)

  foo := new_thing()
  expect_equal(foo, list(name = "foo", value = NULL))

  # positional arguments shift to the remaining parameters
  bar := new_thing(1)
  expect_equal(bar, list(name = "bar", value = 1))
})

test_that(":= returns the value invisibly", {
  new_thing <- function(name) list(name = name)
  expect_invisible(foo := new_thing())
})

test_that(":= validates its inputs", {
  new_thing <- function(name) list(name = name)
  no_name <- function() "x"

  expect_snapshot(error = TRUE, {
    "foo" := new_thing()
    foo := 10
    foo := new_thing(name = "bar")
    foo := no_name()
  })
})
