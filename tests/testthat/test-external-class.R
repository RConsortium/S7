test_that("displays nicely", {
  foo <- new_external_class("package", "name", function() new_class("foo"))
  expect_snapshot(print(foo))
})

test_that("gets updated constructor", {
  foo1 <- new_class("foo", properties = list(x = class_double))
  foo1_ex <- new_external_class("S7", "foo1", function() foo1)
  foo2 <- new_class("foo2", parent = foo1_ex)

  foo1 <- new_class(
    "foo",
    properties = list(x = class_double),
    constructor = function(x) {
      new_object(S7_object, x = x + 1)
    }
  )

  f <- foo2(x = 1)
  expect_equal(f@x, 2)
})

test_that("gets updated validator", {
  foo1 <- new_class("foo", properties = list(x = class_double))
  foo1_ex <- new_external_class("S7", "foo1", function() foo1)
  foo2 <- new_class("foo2", parent = foo1_ex)

  foo1 <- new_class(
    "foo",
    properties = list(x = class_double),
    validator = function(self) {
      if (self@x < 0) {
        "@x must be positive"
      }
    }
  )
  expect_error(foo2(x = -1), "invalid")
})

test_that("gets updated properties", {
  foo1 <- new_class("foo", properties = list(x = class_double))
  foo1_ex <- new_external_class("S7", "foo1", function() foo1)
  foo2 <- new_class("foo2", parent = foo1_ex)

  foo1 <- new_class("foo", properties = list(x = class_double, y = class_double))
  f <- foo2()
  expect_equal(prop_names(f), c("x", "y"))
})


test_that("constructor_fun must yield a class", {
  expect_snapshot(error = TRUE, {
    new_external_class("S7", "foo", function() 1)
  })
})
