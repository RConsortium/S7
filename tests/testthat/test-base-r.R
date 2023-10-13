

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

test_that("dput(<S7_object>) works", {
  skip_if(getRversion() < "4.3")
  skip("dput() not fixed yet; https://github.com/RConsortium/S7/issues/272")

  expect_no_error(dput(new_class("Foo")()))
  expect_no_error(dput(new_class("Foo")))
})
