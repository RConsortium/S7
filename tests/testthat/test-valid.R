test_that("validate() validates object and type recursively", {
  klass := new_class(
    package = NULL,
    properties = list(x = class_double, y = class_double),
    validator = function(self) {
      c(
        if (self@x < 0) "x must be positive",
        if (self@y > 0) "y must be negative"
      )
    }
  )

  expect_snapshot(error = TRUE, {
    obj <- klass(1, -1)
    attr(obj, "x") <- -1
    validate(obj)

    attr(obj, "x") <- "y"
    validate(obj)
  })

  klass2 := new_class(
    parent = klass,
    package = NULL,
    properties = list(z = class_double)
  )
  expect_snapshot(error = TRUE, {
    obj <- klass2(1, -1, 1)
    attr(obj, "x") <- -1
    validate(obj)

    attr(obj, "x") <- "y"
    attr(obj, "z") <- "y"
    validate(obj)
  })
})

test_that("validate checks base type", {
  Double := new_class(package = NULL, parent = class_double)
  x <- Double(10)
  mode(x) <- "character"

  expect_snapshot(error = TRUE, validate(x))
})

test_that("validate checks the type of setters", {
  foo := new_class(
    package = NULL,
    properties = list(
      x = new_property(
        class_double,
        setter = function(self, value) {
          self@x <- as.character(value)
          self
        }
      )
    )
  )
  expect_snapshot(foo(x = 123), error = TRUE)
})

test_that("validate does not check type of getters", {
  # because getters can be peform arbitrary computation and we want
  # validation to always be cheap

  prop <- new_property(class_integer, getter = function(self) "x")
  foo := new_class(properties = list(x = prop))

  expect_no_error(foo())
})

test_that("valid eventually calls the validation function only at the end", {
  foo := new_class(
    properties = list(x = class_double),
    validator = function(self) if (self@x < 0) "must be positive"
  )
  obj <- foo(10)

  obj <- valid_eventually(obj, function(self) {
    self@x <- -1
    self@x <- 1
    self
  })
  expect_error(validate(obj), NA)
})

test_that("valid implicitly does _not_ call the validation function", {
  foo := new_class(
    properties = list(x = class_double),
    validator = function(self) if (self@x < 0) "must be positive"
  )
  obj <- foo(10)

  obj <- valid_implicitly(obj, function(self) {
    self@x <- -1
    self
  })
  expect_error(validate(obj), "must be positive")
})


test_that("property validation errors have class S7_error_validation_failed", {
  klass := new_class(
    package = NULL,
    properties = list(x = class_double),
    validator = function(self) if (self@x < 0) "x must be positive"
  )
  obj <- klass(1)
  attr(obj, "x") <- "bad"
  expect_error(validate(obj), class = "S7_error_validation_failed")
})

test_that("class validator errors have class S7_error_validation_failed", {
  klass := new_class(
    package = NULL,
    properties = list(x = class_double),
    validator = function(self) if (self@x < 0) "x must be positive"
  )
  obj <- klass(1)
  attr(obj, "x") <- -1
  expect_error(validate(obj), class = "S7_error_validation_failed")
})

test_that("inherited property validators run only once during construction", {
  x_calls <- 0L
  parent <- new_class(
    "parent",
    package = NULL,
    properties = list(
      x = new_property(
        validator = function(value) {
          x_calls <<- x_calls + 1L
          NULL
        },
        default = 1
      )
    )
  )
  child <- new_class("child", package = NULL, parent = parent)
  grandchild <- new_class("grandchild", package = NULL, parent = child)

  x_calls <- 0L
  parent()
  expect_equal(x_calls, 1L)

  x_calls <- 0L
  child()
  expect_equal(x_calls, 1L)

  x_calls <- 0L
  grandchild()
  expect_equal(x_calls, 1L)
})

test_that("overridden property validators run during construction", {
  parent_calls <- 0L
  child_calls <- 0L
  parent <- new_class(
    "parent",
    package = NULL,
    properties = list(
      x = new_property(
        validator = function(value) {
          parent_calls <<- parent_calls + 1L
          NULL
        },
        default = 1
      )
    )
  )
  child <- new_class(
    "child",
    package = NULL,
    parent = parent,
    properties = list(
      x = new_property(
        validator = function(value) {
          child_calls <<- child_calls + 1L
          NULL
        },
        default = 1
      )
    )
  )

  parent_calls <- 0L
  child_calls <- 0L
  child()
  expect_equal(parent_calls, 1L)
  expect_equal(child_calls, 1L)
})
