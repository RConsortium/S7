test_that("validation gives useful message", {
  expect_equal(base_classes$integer$validator(1L), NULL)
  expect_snapshot(base_classes$integer$validator(TRUE))
})

test_that("base class display as expected", {
  expect_snapshot({
    base_classes
    str(base_classes)
  })
})

test_that("classes can inherit from base types", {
  for (class in base_classes) {
    foo <- new_class("foo", parent = class)
    expect_error(foo(), NA)
  }
})

describe("environments", {
  it("has reference semantics", {
    env <- new_class("env", "environment", properties = list(x = double))
    x1 <- x2 <- env(x = 1)
    x1@x <- 2
    expect_equal(x2@x, 2)
  })

  it("can be printed", {
    env <- new_class("env", "environment", properties = list(x = double))
    expect_snapshot(env(x = 1), transform = scrub_environment)
  })
})
