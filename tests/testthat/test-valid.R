test_that("validate() validates object and type recursively", {
  klass <- new_class("klass", package = NULL,
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

  klass2 <- new_class("klass2", parent = klass, package = NULL,
                      properties = list(z = class_double))
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
  Double <- new_class("Double", package = NULL, parent = class_double)
  x <- Double(10)
  mode(x) <- "character"

  expect_snapshot(error = TRUE, validate(x))
})

test_that("validate checks the type of setters", {
  foo <- new_class("foo", package = NULL, properties = list(x =
    new_property(
      class_double,
      setter = function(self, value) {
        self@x <- as.character(value)
        self
      }
    )
  ))
  expect_snapshot(foo(x = 123), error = TRUE)
})

test_that("validate does not check type of getters", {
  # because getters can be peform arbitrary computation and we want
  # validation to always be cheap

  prop <- new_property(class_integer, getter = function(self) "x")
  foo <- new_class("foo", properties =  list(x = prop))

  expect_no_error(foo())
})

test_that("valid eventually calls the validation function only at the end", {
  foo <- new_class("foo",
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
  foo <- new_class("foo",
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


describe("validation error classes", {
  klass <- new_class("klass", package = NULL,
    properties = list(x = class_double),
    validator = function(self) if (self@x < 0) "x must be positive"
  )

  it("property errors have class S7_error_validation_property", {
    obj <- klass(1)
    attr(obj, "x") <- "bad"

    cnd <- tryCatch(validate(obj), S7_error_validation_property = identity)
    expect_s3_class(cnd, c("S7_error_validation_property", "S7_error_validation", "error", "condition"), exact = TRUE)
    expect_equal(cnd$object_class, "<klass>")
    expect_equal(cnd$errors, "@x must be <double>, not <character>")
    expect_null(cnd$call)
  })

  it("custom validator errors have class S7_error_validation_object", {
    obj <- klass(1)
    attr(obj, "x") <- -1

    cnd <- tryCatch(validate(obj), S7_error_validation_object = identity)
    expect_s3_class(cnd, c("S7_error_validation_object", "S7_error_validation", "error", "condition"), exact = TRUE)
    expect_equal(cnd$object_class, "<klass>")
    expect_equal(cnd$errors, "x must be positive")
    expect_null(cnd$call)
  })

  it("both are catchable via the parent class S7_error_validation", {
    obj <- klass(1)
    attr(obj, "x") <- "bad"
    cnd1 <- tryCatch(validate(obj), S7_error_validation = identity)
    expect_s3_class(cnd1, "S7_error_validation_property")

    obj2 <- klass(1)
    attr(obj2, "x") <- -1
    cnd2 <- tryCatch(validate(obj2), S7_error_validation = identity)
    expect_s3_class(cnd2, "S7_error_validation_object")
  })

  it("setter errors also use structured property conditions", {
    obj <- klass(1)

    cnd <- tryCatch({ obj@x <- "bad" }, S7_error_validation_property = identity)
    expect_s3_class(cnd, c("S7_error_validation_property", "S7_error_validation", "error", "condition"), exact = TRUE)
    expect_equal(cnd$object_class, "<klass>")
    expect_equal(cnd$errors, "<klass>@x must be <double>, not <character>")
    expect_null(cnd$call)
  })
})
