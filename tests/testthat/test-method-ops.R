test_that("Ops generics dispatch to S7 methods", {
  skip_if(getRversion() < "4.3")

  ## Test Ops
  ClassX <- new_class("ClassX")
  method(`+`, list(class_any, ClassX))   <- function(e1, e2) "class_any + ClassX"
  method(`+`, list(ClassX, class_any))   <- function(e1, e2) "ClassX + class_any"
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
  matrixOps.bar   <- function(x, y) paste(class(x), .Generic, class(y))

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

