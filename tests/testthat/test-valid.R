test_that("validate calls the validation function", {
  obj <- range(1, 10)
  # Use attr to set the property
  attr(obj, "start") <- 11

  expect_error(
    validate(obj),
    "must be greater than"
  )
})

test_that("valid eventually calls the validation function only at the end", {
  obj <- range(1, 10)

  obj <- valid_eventually(
    obj,
    function(x) {
      x@start <- 11
      x@start <- 1
      x
    })

  expect_error(validate(obj), NA)
})

test_that("valid implicitly does _not_ call the validation function", {
  obj <- range(1, 10)

  obj <- valid_implicitly(
    obj,
    function(x) {
      x@start <- 11
      x
    })

  expect_error(validate(obj),
    "must be greater than"
  )
})
