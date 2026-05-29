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
