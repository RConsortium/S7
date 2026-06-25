test_that("base condition classes have the right class vectors", {
  expect_equal(class(class_construct(class_condition)), "condition")
  expect_equal(class(class_construct(class_error)), c("error", "condition"))
  expect_equal(
    class(class_construct(class_warning)),
    c("warning", "condition")
  )
})

test_that("can construct condition subclasses with S7 properties", {
  my_error := new_class(
    parent = class_error,
    properties = list(data = class_numeric),
    package = NULL
  )
  x <- my_error(message = "boom", data = 1:3)

  expect_equal(class(x), c("my_error", "error", "condition", "S7_object"))
  expect_equal(conditionMessage(x), "boom")
  expect_equal(x@data, 1:3)
  expect_equal(validate(x), x)

  caught <- tryCatch(stop(x), my_error = function(e) e)
  expect_equal(conditionMessage(caught), "boom")
  expect_equal(caught@data, 1:3)
})

test_that("catches invalid conditions", {
  expect_snapshot({
    validate_condition(1)
    validate_condition(structure(list(), class = "condition"))
    validate_condition(structure(list(message = 1), class = "condition"))
  })
  expect_null(validate_condition(simpleError("boom")))
})
