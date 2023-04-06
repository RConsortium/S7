

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
  skip_if(getRversion() < "4.3")

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
  skip_if(getRversion() < "4.3")

  ## Test Ops
  ClassX <- new_class("ClassX")
  method(`+`, list(class_any, ClassX))   <- function(x, y) "class_any + ClassX"
  method(`+`, list(ClassX, class_any))   <- function(x, y) "ClassX + class_any"
  method(`%*%`, list(ClassX, class_any)) <- function(x, y) "ClassX %*% class_any"
  method(`%*%`, list(class_any, ClassX)) <- function(x, y) "class_any %*% ClassX"

  on.exit(S4_remove_classes(c("an_s4_class")))
  methods::setClass("an_s4_class", contains = "character", where = globalenv())
  an_s4_obj <- methods::new(methods::getClass("an_s4_class"))

  test_vals <- list(1, NULL, new_class("ClassA")(), an_s4_obj,
                    Sys.time(), structure("", class = "foo"))

  val <- 1
  identical(ClassX() + val,   "ClassX + class_any")
  identical(val + ClassX(),   "class_any + ClassX")
  identical(ClassX() %*% val, "ClassX %*% class_any")
  identical(val %*% ClassX(), "class_any %*% ClassX")

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
  `+.foo`   <- function(e1, e2) paste(class(e1), "+"     , class(e2))
  `%*%.foo` <- function(x, y)   paste(class(x) , "%*%"   , class(y))
  Ops.bar   <- function(e1, e2) paste(class(e1), .Generic, class(e2))
  matrixOps.bar   <- function(e1, e2) paste(class(e1), .Generic, class(e2))

  foo <- structure("", class = "foo")
  bar <- structure("", class = "bar")
  expect_no_error(stopifnot(exprs = {
    identical(foo %*% 1, "foo %*% numeric")
    identical(1 %*% foo, "numeric %*% foo")

    identical(bar %*% 1, "bar %*% numeric")
    identical(1 %*% bar, "numeric %*% bar")

    identical(foo + 1, "foo + numeric")
    identical(1 + foo, "numeric + foo")

    identical(bar + 1, "bar + numeric")
    identical(1 + bar, "numeric + bar")
  }))

})


test_that("dput(<S7_object>) works", {
  skip_if(getRversion() < "4.3")
  skip("dput() not fixed yet; https://github.com/RConsortium/OOP-WG/issues/272")

  expect_no_error(dput(new_class("Foo")()))
  expect_no_error(dput(new_class("Foo")))
})
