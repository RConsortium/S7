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

test_that("super dispatch does not add its marker to user frames", {
  foo := new_generic("x")
  Base := new_class()
  Child := new_class(parent = Base)

  method(foo, Base) <- function(x) {
    list(
      method = environment(),
      generic = parent.frame(),
      caller = S7_user_frame(skip = "super")
    )
  }
  method(foo, Child) <- function(x) foo(super(x, Base))

  frames <- foo(Child())
  expect_identical(frames$caller, environment())
  expect_equal(
    vapply(
      frames,
      \(env) exists("_dispatched_super", envir = env, inherits = FALSE),
      logical(1)
    ),
    c(method = FALSE, generic = FALSE, caller = FALSE)
  )
})

test_that("super dispatch preserves generic locals", {
  foo := new_generic("x", function(x) {
    `_dispatched_super` <- "user value"
    inside <- S7_dispatch()
    list(inside = inside, after = `_dispatched_super`)
  })
  Base := new_class()
  method(foo, Base) <- function(x) {
    get("_dispatched_super", envir = parent.frame(), inherits = FALSE)
  }

  expect_equal(
    foo(super(Base(), Base)),
    list(inside = "user value", after = "user value")
  )
})

test_that("successive dispatches do not share the super marker", {
  Base := new_class()
  Child := new_class(parent = Base)
  foo := new_generic("x", function(x) {
    first <- S7_dispatch()
    if (!inherits(x, "S7_super")) {
      return(first)
    }
    x <- Base()
    list(first = first, second = S7_dispatch())
  })
  method(foo, Base) <- method_context
  method(foo, Child) <- function(x) {
    sentinel <- "method"
    foo(super(x, Base))
  }

  sentinel <- "caller"
  expect_equal(
    foo(Child()),
    list(
      first = list(call = quote(foo(Child())), sentinel = "caller"),
      second = list(call = quote(foo(super(x, Base))), sentinel = "method")
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
  foo := new_generic("x", function(x) {
    `_dispatched_super` <- "user value"
    S7_dispatch()
  })

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
