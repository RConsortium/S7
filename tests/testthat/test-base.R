test_that("validation uses typeof", {
  expect_equal(class_integer$validator(1L), NULL)
  expect_equal(class_integer$validator(factor()), NULL)
  expect_snapshot(class_integer$validator(TRUE))

  expect_equal(class_function$validator(`[`), NULL)
  expect_equal(class_function$validator(sum), NULL)
  expect_equal(class_function$validator(mean), NULL)
})

test_that("base class display as expected", {
  expect_snapshot({
    base_classes
    str(base_classes)
  })
})

test_that("classes can inherit from base types", {
  base_classes <- c(class_vector$classes, list(class_function, class_environment))

  for (class in base_classes) {
    foo <- new_class("foo", parent = class)
    expect_error(foo(), NA)
  }
})

describe("environments", {
  it("has reference semantics", {
    env <- new_class("env", class_environment, properties = list(x = class_double))
    x1 <- x2 <- env(x = 1)
    x1@x <- 2
    expect_equal(x2@x, 2)
  })

  it("can be printed", {
    env <- new_class("env", class_environment, properties = list(x = class_double))
    expect_snapshot(env(x = 1), transform = scrub_environment)
  })
})
