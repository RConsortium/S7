test_that("S7_user_frame() can skip super() to return the original caller", {
  foo := new_generic("x")

  x <- 1
  method(foo, class_double) <- function(x) {
    eval(quote(x), S7_user_frame(skip = "super"))
  }

  expect_equal(foo(1), 1)
  local({
    x <- 2
    expect_equal(foo(1), 2)
  })

  # Even in the presence of super()
  Number := new_class(parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))

  expect_equal(foo(Number(1)), 1)
  local({
    x <- 2
    expect_equal(foo(Number(1)), 2)
  })
})

test_that("S7_user_frame() stops at super() by default", {
  foo := new_generic("x")
  Number := new_class(parent = class_double)

  method(foo, class_double) <- function(x) {
    list(
      default = eval(quote(sentinel), S7_user_frame()),
      none = eval(quote(sentinel), S7_user_frame(skip = "none"))
    )
  }
  method(foo, Number) <- function(x) {
    sentinel <- "method"
    foo(super(x, class_double))
  }

  sentinel <- "caller"
  expect_equal(foo(Number(1)), list(default = "method", none = "method"))
})

test_that("S7_generic_call() can skip super() to return the original call", {
  foo := new_generic("x")
  method(foo, class_double) <- function(x) S7_generic_call(skip = "super")
  expect_equal(foo(1), quote(foo(1)))

  # Even in the presence of super()
  Number := new_class(parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))
  expect_equal(foo(Number(1)), quote(foo(Number(1))))
})

test_that("S7_generic_call() stops at super() by default", {
  foo := new_generic("x")
  Number := new_class(parent = class_double)

  method(foo, class_double) <- function(x) {
    list(default = S7_generic_call(), none = S7_generic_call(skip = "none"))
  }
  method(foo, Number) <- function(x) foo(super(x, class_double))
  expect_equal(
    foo(Number(1)),
    list(
      default = quote(foo(super(x, class_double))),
      none = quote(foo(super(x, class_double)))
    )
  )
})

test_that("helpers reject unknown skip values", {
  expect_snapshot(error = TRUE, {
    S7_generic_call(skip = "invalid")
    S7_user_frame(skip = "invalid")
  })
})

test_that("helpers work from functions called by a method", {
  foo := new_generic("x")
  helper <- function() S7_generic_call()
  method(foo, class_double) <- function(x) helper()
  expect_equal(foo(1), quote(foo(1)))
})

test_that("S7_generic_fun() returns the generic being dispatched", {
  foo := new_generic("x")
  method(foo, class_double) <- function(x) S7_generic_fun()
  expect_identical(foo(1), foo)

  # Even in the presence of super()
  Number := new_class(parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))
  expect_identical(foo(Number(1)), foo)
})

test_that("S7_generic_fun() returns the nearest generic", {
  inner := new_generic("x")
  outer := new_generic("x")

  method(inner, class_double) <- function(x) S7_generic_fun()
  method(outer, class_double) <- function(x) {
    list(inner = inner(x), outer = S7_generic_fun())
  }
  expect_equal(outer(1), list(inner = inner, outer = outer))
})

test_that("super redispatch through helpers reports original generic context", {
  foo := new_generic("x")
  Number := new_class(parent = class_double)

  method(foo, class_double) <- method_context
  redispatch <- function(x) {
    sentinel <- "helper"
    foo(super(x, class_double))
  }
  method(foo, Number) <- function(x) {
    sentinel <- "method"
    redispatch(x)
  }

  sentinel <- "caller"
  expect_equal(
    foo(Number(1)),
    list(call = quote(foo(Number(1))), sentinel = "caller")
  )
})

test_that("S7_generic_call(match = TRUE) names the arguments", {
  foo := new_generic("x")
  method(foo, class_double) <- function(x) S7_generic_call(match = TRUE)
  expect_equal(foo(1), quote(foo(x = 1)))
})

test_that("a different nested generic stops the walk (nearest generic)", {
  inner := new_generic("x")
  outer := new_generic("x")

  method(inner, class_double) <- function(x) {
    S7_generic_call(skip = "super")
  }
  method(outer, class_double) <- function(x) {
    list(
      inner = inner(x),
      outer = S7_generic_call(skip = "super")
    )
  }
  expect_equal(outer(1), list(inner = quote(inner(x)), outer = quote(outer(1))))
})

test_that("super() passed to a different generic stops the walk", {
  inner := new_generic("x")
  outer := new_generic("x")
  Number := new_class(parent = class_double)

  method(inner, class_double) <- method_context
  method(outer, Number) <- function(x) {
    sentinel <- "outer method"
    inner(super(x, class_double))
  }

  sentinel <- "caller"
  expect_equal(
    outer(Number(1)),
    list(
      call = quote(inner(super(x, class_double))),
      sentinel = "outer method"
    )
  )
})

test_that("intervening generic stops same-generic super walk", {
  foo := new_generic("x")
  bar := new_generic("x")
  Number := new_class(parent = class_double)

  method(foo, class_double) <- method_context
  method(foo, Number) <- function(x) {
    sentinel <- "foo method"
    bar(x)
  }
  method(bar, Number) <- function(x) {
    sentinel <- "bar method"
    foo(super(x, class_double))
  }

  sentinel <- "caller"
  expect_equal(
    foo(Number(1)),
    list(
      call = quote(foo(super(x, class_double))),
      sentinel = "bar method"
    )
  )
})

test_that("same-generic nested calls are not super redispatches", {
  foo := new_generic("x")

  method(foo, class_double) <- function(x) {
    sentinel <- "method frame"
    foo("inner")
  }
  method(foo, class_character) <- method_context

  sentinel <- "caller frame"
  expect_equal(
    foo(1),
    list(call = quote(foo("inner")), sentinel = "method frame")
  )
})

test_that("helpers error when called outside a method", {
  expect_snapshot(error = TRUE, {
    S7_generic_call()
    S7_user_frame()
    S7_generic_fun()
  })
})

test_that("helpers error from generic bodies outside active methods", {
  before := new_generic("x", function(x) {
    S7_generic_call()
    S7_dispatch()
  })
  method(before, class_double) <- function(x) x
  expect_snapshot(error = TRUE, before(1))

  after := new_generic("x", function(x) {
    S7_dispatch()
    S7_user_frame()
  })
  method(after, class_double) <- function(x) x
  expect_snapshot(error = TRUE, after(1))
})

test_that("helpers error in methods not dispatched by an S7 generic", {
  Foo := new_class(package = NULL)
  method(`+`, list(Foo, Foo)) <- function(e1, e2) S7_generic_call()

  outer := new_generic("x")
  method(outer, class_double) <- function(x) Foo() + Foo()

  expect_snapshot(error = TRUE, outer(1))
})

test_that("helpers error while forcing dispatch arguments", {
  foo := new_generic("x")
  method(foo, class_double) <- function(x) x

  expect_snapshot(error = TRUE, {
    foo({
      S7_generic_call()
      1
    })
  })
})
