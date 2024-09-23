test_that("list2() works", {
  # list2() is equivalent to base::list(), with the following differences:
  # - A missing arg value is silently ignored instead of signaling an error.
  # - An argument is automatically named if it is unnamed and the value expression is a symbol.

  expect_identical(list2(), list())
  expect_identical(list2(a = 1),             list(a = 1))
  expect_identical(list2(a = 1, b = ),       list(a = 1))
  expect_identical(list2(a = 1, b = , , ),   list(a = 1))
  expect_identical(list2(, a = 1, b = , , ), list(a = 1))
  a <- 1
  expect_identical(list2(a),               list(a = 1))
  expect_identical(list2(a, b = ),         list(a = 1))
  expect_identical(list2(a, b = , a, ),    list(a = 1, a = 1))
  expect_identical(list2(a = identity(a)), list(a = 1))

  expect_identical(list2((a)),             list(1))
  expect_identical(list2(identity(a)),     list(1))

  # make sure all this works if values in `...` are nested promises
  f1 <- function(...) list2(...)
  f2 <- function(..., b) f1(..., b)
  f3 <- function(..., c) f2(..., c)
  f4 <- function(..., d) f3(..., d)

  a <- 1; b <- 2
  for (f in list(f1, f2, f3, f4, list2)) {
    expect_identical(f(), list())
    expect_mapequal(f(a = 1),        list(a = 1))
    expect_mapequal(f(a = 1, b =),   list(a = 1))
    expect_mapequal(f(a = 1, b = 2), list(a = 1, b = 2))
    expect_mapequal(f(a, b),         list(a = 1, b = 2))
  }

  expect_identical(list2(a, b, a + b),       list(a = 1, b = 2,     3))
  expect_identical(list2(a, b, c = a + b),   list(a = 1, b = 2, c = 3))
  expect_identical(list2((a), b, c = a + b), list(    1, b = 2, c = 3))
})
