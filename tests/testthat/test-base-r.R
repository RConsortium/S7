

test_that("base::inherits() accepts S7 objects", {

  skip_if(getRversion() < "4.3")

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

})


test_that("base::`@` accesses S7 properties", {

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
    `@` <- base::`@`
    expect_no_error(stopifnot(exprs = {
      identical(obj@start, 3)
      identical(obj@end, 4)
    }))

  })

})


test_that("Ops generics dispatch to S7 methods", {

  ## Test Ops
  ClassX <- new_class("ClassX")
  method(`+`, list(class_any, ClassX))   <- function(x, y) "class_any + ClassX"
  method(`+`, list(ClassX, class_any))   <- function(x, y) "ClassX + class_any"
  method(`%*%`, list(ClassX, class_any)) <- function(x, y) "ClassX %*% class_any"
  method(`%*%`, list(class_any, ClassX)) <- function(x, y) "class_any %*% ClassX"

  test_vals <- list(1, NULL, new_class("ClassA")(),
                    Sys.time(), structure("", class = "foo"))

  for (val in test_vals)
    expect_no_error(stopifnot(exprs = {
      identical(ClassX() + val,   "ClassX + class_any")
      identical(val + ClassX(),   "class_any + ClassX")
      identical(ClassX() %*% val, "ClassX %*% class_any")
      identical(val %*% ClassX(), "class_any %*% ClassX")
    }))

  expect_no_error(stopifnot(exprs = {
    identical(ClassX()  +  ClassX(), "ClassX + class_any")
    identical(ClassX() %*% ClassX(), "ClassX %*% class_any")
  }))

  # S3 dispatch still works
  `%*%.foo` <- function(x, y) paste(class(x), "%*%", class(y))
  Ops.bar   <- function(x, y) paste(class(x), .Generic, class(y))

  foo <- structure("", class = "foo")
  bar <- structure("", class = "bar")
  expect_no_error(stopifnot(exprs = {
    identical(foo %*% 1, "foo %*% numeric")
    identical(1 %*% foo, "numeric %*% foo")

    identical(bar %*% 1, "bar %*% numeric")
    identical(1 %*% bar, "numeric %*% bar")
  }))

})
