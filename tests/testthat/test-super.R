
describe("super()", {
  it("overrides dispatch, matching inherited behaviour", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)
    foo3 <- new_class("foo3", foo2)

    bar <- new_generic("bar", "x")
    method(bar, foo1) <- function(x) 1
    method(bar, foo3) <- function(x) 3

    expect_equal(bar(super(foo3(), to = foo2)), 1)
    expect_equal(bar(super(foo3(), to = foo1)), 1)
  })

  it("only affects one dispatch", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    bar1 <- new_generic("bar1", "x")
    method(bar1, foo1) <- function(x) 1
    method(bar1, foo2) <- function(x) 2

    bar2 <- new_generic("bar2", "x")
    method(bar2, foo1) <- function(x) c(1, bar1(x))
    method(bar2, foo2) <- function(x) c(2, bar1(x))

    expect_equal(bar2(super(foo2(), to = foo1)), c(1, 2))
    expect_equal(bar2(convert(foo2(), to = foo1)), c(1, 1))
  })

  it("checks to", {
    expect_snapshot(error = TRUE, {
      foo <- new_class("foo")
      super(foo(), class_character)
    })
  })

  it("displays nicely", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    expect_snapshot({
      f1 <- super(foo2(), foo1)
      f1
      str(list(f1))
    })

  })
})
