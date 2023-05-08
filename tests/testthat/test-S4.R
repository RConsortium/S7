test_that("can work with classGenerators", {
  on.exit(S4_remove_classes("Foo"))
  Foo <- setClass("Foo", where = globalenv())
  expect_equal(S4_to_S7_class(Foo), getClass("Foo"))
})

test_that("converts S4 base classes to S7 base classes", {
  expect_equal(S4_to_S7_class(getClass("NULL")), NULL)
  expect_equal(S4_to_S7_class(getClass("character")), class_character)
})

test_that("converts S4 unions to S7 unions", {
  on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3", "Union1", "Union2")))

  setClass("Foo1", slots = "x", where = globalenv())
  setClass("Foo2", slots = "x", where = globalenv())

  setClassUnion("Union1", c("Foo1", "Foo2"), where = globalenv())
  expect_equal(
    S4_to_S7_class(getClass("Union1")),
    new_union(getClass("Foo1"), getClass("Foo2"))
  )

  setClass("Foo3", slots = "x", where = globalenv())
  setClassUnion("Union2", c("Union1", "Foo3"), where = globalenv())
  expect_equal(
    S4_to_S7_class(getClass("Union2")),
    new_union(getClass("Foo1"), getClass("Foo2"), getClass("Foo3"))
  )
})

test_that("converts S4 representation of S3 classes to S7 representation", {
  expect_equal(S4_to_S7_class(getClass("Date")), new_S3_class("Date"), ignore_function_env = TRUE)
})

test_that("errors on non-S4 classes", {
  expect_snapshot(S4_to_S7_class(1), error = TRUE)
})


describe("S4_class_dispatch", {
  it("returns name of base class", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", slots = list("x" = "numeric"), where = globalenv())
    expect_equal(S4_class_dispatch("Foo1"), "S4/Foo1")
  })

  it("respects single inheritance hierarchy", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2","Foo3")))

    setClass("Foo1", slots = list("x" = "numeric"), where = globalenv())
    setClass("Foo2", contains = "Foo1", where = globalenv())
    setClass("Foo3", contains = "Foo2", where = globalenv())
    expect_equal(S4_class_dispatch("Foo3"), c("S4/Foo3", "S4/Foo2", "S4/Foo1"))
  })

  it("performs breadth first search for multiple dispatch", {
    on.exit(S4_remove_classes(c("Foo1a", "Foo1b","Foo2a", "Foo2b", "Foo3")))
    setClass("Foo1a", slots = list("x" = "numeric"), where = globalenv())
    setClass("Foo1b", contains = "Foo1a", where = globalenv())
    setClass("Foo2a", slots = list("x" = "numeric"), where = globalenv())
    setClass("Foo2b", contains = "Foo2a", where = globalenv())
    setClass("Foo3", contains = c("Foo1b", "Foo2b"), where = globalenv())
    expect_equal(
      S4_class_dispatch("Foo3"),
      c("S4/Foo3", "S4/Foo1b", "S4/Foo2b", "S4/Foo1a", "S4/Foo2a")
    )
  })

  it("handles extensions of base classes", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", contains = "character", where = globalenv())
    expect_equal(S4_class_dispatch("Foo1"), c("S4/Foo1", "character"))
  })

  it("handles extensions of S3 classes", {
    on.exit(S4_remove_classes(c("Soo1", "Foo2", "Foo3")))

    setOldClass(c("Soo1", "Soo"), where = globalenv())
    setClass("Foo2", contains = "Soo1", where = globalenv())
    setClass("Foo3", contains = "Foo2", where = globalenv())
    expect_equal(S4_class_dispatch("Foo3"), c("S4/Foo3", "S4/Foo2", "Soo1", "Soo"))
  })

  it("ignores unions", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3")))

    setClass("Foo1", slots = list("x" = "numeric"), where = globalenv())
    setClass("Foo2", slots = list("x" = "numeric"), where = globalenv())
    setClassUnion("Foo3", c("Foo1", "Foo2"), where = globalenv())

    expect_equal(S4_class_dispatch("Foo1"), "S4/Foo1")
    expect_equal(S4_class_dispatch("Foo2"), "S4/Foo2")
  })

  it("includes virtual classes", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2")))

    setClass("Foo1", where = globalenv())
    setClass("Foo2", contains = "Foo1", where = globalenv())

    expect_equal(S4_class_dispatch("Foo1"), "S4/Foo1")
    expect_equal(S4_class_dispatch("Foo2"), c("S4/Foo2", "S4/Foo1"))
  })

  it("captures explicit package name", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", package = "pkg", where = globalenv())
    expect_equal(S4_class_dispatch("Foo1"), "S4/pkg::Foo1")
  })

  it("captures implicit package name", {
    on.exit(S4_remove_classes("Foo1", env))

    env <- new.env()
    env$.packageName <- "mypkg"
    setClass("Foo1", where = env)
    expect_equal(S4_class_dispatch("Foo1"), "S4/mypkg::Foo1")
  })
})
