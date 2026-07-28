test_that("in_dev() is FALSE for packages not under development", {
  local_end_user()
  method <- function_in_package("methodPkg")
  generic <- function_in_package("genericPkg")
  expect_false(in_dev(method, generic))

  # but methods defined outside of a package are always in development
  expect_true(in_dev(function_in_package(NULL), generic))
})

test_that("in_dev() is TRUE while load_all() is active", {
  local_load_all()
  method <- function_in_package("methodPkg")
  generic <- function_in_package("genericPkg")
  expect_true(in_dev(method, generic))
})

test_that("in_dev() is TRUE when R CMD check checks an involved package", {
  method <- function_in_package("methodPkg")
  generic <- function_in_package("genericPkg")
  signature <- list(structure(list(), package = "classPkg"))

  local_R_CMD_check("methodPkg")
  expect_true(in_dev(method, generic, signature))

  local_R_CMD_check("genericPkg")
  expect_true(in_dev(method, generic, signature))

  local_R_CMD_check("classPkg")
  expect_true(in_dev(method, generic, signature))

  local_R_CMD_check("otherPkg")
  expect_false(in_dev(method, generic, signature))
})

test_that("in_dev() is FALSE inside package tests, even under R CMD check", {
  local_envvar(
    DEVTOOLS_LOAD = NA,
    TESTTHAT = "true",
    "_R_CHECK_PACKAGE_NAME_" = "methodPkg"
  )
  method <- function_in_package("methodPkg")
  generic <- function_in_package("genericPkg")
  expect_false(in_dev(method, generic))
})

test_that("in_dev() with no arguments only detects load_all()", {
  local_end_user()
  expect_false(in_dev())

  local_load_all()
  expect_true(in_dev())
})
