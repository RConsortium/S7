method_context <- function(x) {
  list(
    call = S7_generic_call(),
    sentinel = eval(quote(sentinel), S7_user_frame())
  )
}

test_that("S7_user_frame() returns the calling frame of the generic", {
  foo <- new_generic("foo", "x")

  x <- 1
  method(foo, class_double) <- function(x) eval(quote(x), S7_user_frame())

  expect_equal(foo(1), 1)
  local({
    x <- 2
    expect_equal(foo(1), 2)
  })

  # Even in the presence of super()
  Number <- new_class("Number", parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))

  expect_equal(foo(Number(1)), 1)
  local({
    x <- 2
    expect_equal(foo(Number(1)), 2)
  })
})

test_that("S7_generic_call() is the originating call to the generic", {
  foo <- new_generic("foo", "x")
  method(foo, class_double) <- function(x) S7_generic_call()
  expect_equal(foo(1), quote(foo(1)))

  # Even in the presence of super()
  Number <- new_class("Number", parent = class_double)
  method(foo, Number) <- function(x) foo(super(x, class_double))
  expect_equal(foo(Number(1)), quote(foo(Number(1))))
})

test_that("super redispatch through helpers reports original generic context", {
  foo <- new_generic("foo", "x")
  Number <- new_class("Number", parent = class_double)

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
  foo <- new_generic("foo", "x")
  method(foo, class_double) <- function(x) S7_generic_call(match = TRUE)
  expect_equal(foo(1), quote(foo(x = 1)))
})

test_that("a different nested generic stops the walk (nearest generic)", {
  inner <- new_generic("inner", "x")
  outer <- new_generic("outer", "x")

  method(inner, class_double) <- function(x) {
    S7_generic_call()
  }
  method(outer, class_double) <- function(x) {
    list(
      inner = inner(x),
      outer = S7_generic_call()
    )
  }
  expect_equal(outer(1), list(inner = quote(inner(x)), outer = quote(outer(1))))
})

test_that("super() passed to a different generic stops the walk", {
  inner <- new_generic("inner", "x")
  outer <- new_generic("outer", "x")
  Number <- new_class("Number", parent = class_double)

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
  foo <- new_generic("foo", "x")
  bar <- new_generic("bar", "x")
  Number <- new_class("Number", parent = class_double)

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
  foo <- new_generic("foo", "x")

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
  })
})

test_that("helpers error from generic bodies outside active methods", {
  before <- new_generic("before", "x", function(x) {
    S7_generic_call()
    S7_dispatch()
  })
  method(before, class_double) <- function(x) x
  expect_error(before(1), "Must be called from within a method.", fixed = TRUE)

  after <- new_generic("after", "x", function(x) {
    S7_dispatch()
    S7_user_frame()
  })
  method(after, class_double) <- function(x) x
  expect_error(after(1), "Must be called from within a method.", fixed = TRUE)
})

test_that("helpers error while forcing dispatch arguments", {
  foo <- new_generic("foo", "x")
  method(foo, class_double) <- function(x) x

  expect_error(
    foo({
      S7_generic_call()
      1
    }),
    "Must be called from within a method.",
    fixed = TRUE
  )
})
