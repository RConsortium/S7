test_that("base R is R7 aware", {

  skip_if(getRversion() < "4.3")

  ## Test inherits()
  ClassA <- new_class("ClassA")
  ClassBA <- new_class("ClassBA", parent = ClassA)
  ClassX <- new_class("ClassX")

  expect_no_error(stopifnot(exprs = {
    isTRUE(inherits(ClassA() , ClassA))
    isTRUE(inherits(ClassBA(), ClassA))
    isTRUE(inherits(ClassBA(), ClassBA))

    isFALSE(inherits(ClassX(), ClassA))
    isFALSE(inherits(ClassX(), ClassBA))
  }))


  ## Test @
  range <- new_class(
    "range",
    properties = list(start = class_double,
                      end = class_double),
    validator = function(self) {
      if (length(self@start) != 1) {
        "@start must be length 1"
      } else if (length(self@end) != 1) {
        "@end must be length 1"
      } else if (self@end < self@start) {
        "@end must be greater than or equal to @start"
      }
    }
  )

  obj <- range(3, 4)

  expect_no_error(stopifnot(exprs = {
    identical(obj@start, 3)
    identical(obj@end, 4)
  }))

  local({
    detach("package:R7", character.only = TRUE) # belt
    on.exit(library(R7))
    `@` <- base::`@` # suspenders
    expect_no_error(stopifnot(exprs = {
      identical(obj@start, 3)
      identical(obj@end, 4)
    }))

  })


  ## Test Ops
  ClassX <- new_class("ClassX")
  method(`+`, list(ClassX, class_any)) <-
    function(x, y) "method: ClassX + class_any"

  expect_no_error(stopifnot(exprs = {
    identical(ClassX() + 1,        "method: ClassX + class_any")
    identical(ClassX() + NULL,     "method: ClassX + class_any")
    identical(ClassX() + ClassX(), "method: ClassX + class_any")
    identical(ClassX() + ClassA(), "method: ClassX + class_any")
  }))

  method(`%*%`, list(ClassX, class_any)) <-
    function(x, y) "method: ClassX %*% class_any"

  expect_no_error(stopifnot(exprs = {
    identical(ClassX() %*% 1,        "method: ClassX %*% class_any")
    identical(ClassX() %*% NULL,     "method: ClassX %*% class_any")
    identical(ClassX() %*% ClassX(), "method: ClassX %*% class_any")
    identical(ClassX() %*% ClassA(), "method: ClassX %*% class_any")
  }))

})


