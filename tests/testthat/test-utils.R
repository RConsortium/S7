test_that("collect_dots() collects named dots into a list", {
  expect_equal(collect_dots(x = 1, y = 2), list(x = 1, y = 2))
  expect_equal(collect_dots(), list())
})

test_that("collect_dots() preserves a single unnamed list", {
  expect_equal(collect_dots(list(x = 1, y = 2)), list(x = 1, y = 2))
})

test_that("collect_dots() errors if arguments are unnamed", {
  expect_snapshot(error = TRUE, {
    collect_dots(1, 2)
    collect_dots(list(1, y = 2))
  })
})

test_that("check_function() accepts a matching single signature", {
  expect_snapshot(check_function(1, alist(x = ), arg = "f"), error = TRUE)
  expect_invisible(check_function(function(x) NULL, alist(x = )))
  expect_snapshot(check_function(\(y) {}, alist(x = ), arg = "f"), error = TRUE)
})

test_that("check_function() accepts any of several candidate signatures", {
  sigs <- list(
    alist(self = , value = ),
    alist(self = , name = , value = )
  )
  expect_invisible(check_function(\(self, value) {}, sigs))
  expect_invisible(check_function(\(self, name, value) {}, sigs))

  expect_snapshot(
    check_function(\(x, y, z) {}, sigs, arg = "setter"),
    error = TRUE
  )
})
