test_that("can work with classGenerators", {
  on.exit(S4_remove_classes("Foo"))
  Foo <- setClass("Foo")
  expect_equal(S4_to_S7_class(Foo), getClass("Foo"))
})

test_that("converts S4 base classes to S7 base classes", {
  expect_equal(S4_to_S7_class(getClass("NULL")), NULL)
  expect_equal(S4_to_S7_class(getClass("character")), class_character)
})

test_that("converts S4 unions to S7 unions", {
  on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3", "Union1", "Union2")))

  setClass("Foo1", slots = "x")
  setClass("Foo2", slots = "x")

  setClassUnion("Union1", c("Foo1", "Foo2"))
  expect_equal(
    S4_to_S7_class(getClass("Union1")),
    new_union(getClass("Foo1"), getClass("Foo2"))
  )

  setClass("Foo3", slots = "x")
  setClassUnion("Union2", c("Union1", "Foo3"))
  expect_equal(
    S4_to_S7_class(getClass("Union2")),
    new_union(getClass("Foo1"), getClass("Foo2"), getClass("Foo3"))
  )
})

test_that("converts S4 representation of S3 classes to S7 representation", {
  expect_equal(S4_to_S7_class(getClass("Date")), class_Date, ignore_function_env = TRUE)
})

test_that("errors on non-S4 classes", {
  expect_snapshot(S4_to_S7_class(1), error = TRUE)
})


describe("S4_class_dispatch", {
  it("returns name of base class", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", slots = list("x" = "numeric"))
    expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
  })

  it("respects single inheritance hierarchy", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2","Foo3")))

    setClass("Foo1", slots = list("x" = "numeric"))
    setClass("Foo2", contains = "Foo1")
    setClass("Foo3", contains = "Foo2")
    expect_equal(S4_class_dispatch("Foo3"), c("S4/S7::Foo3", "S4/S7::Foo2", "S4/S7::Foo1"))
  })

  it("performs breadth first search for multiple dispatch", {
    on.exit(S4_remove_classes(c("Foo1a", "Foo1b","Foo2a", "Foo2b", "Foo3")))
    setClass("Foo1a", slots = list("x" = "numeric"))
    setClass("Foo1b", contains = "Foo1a")
    setClass("Foo2a", slots = list("x" = "numeric"))
    setClass("Foo2b", contains = "Foo2a")
    setClass("Foo3", contains = c("Foo1b", "Foo2b"))
    expect_equal(
      S4_class_dispatch("Foo3"),
      c("S4/S7::Foo3", "S4/S7::Foo1b", "S4/S7::Foo2b", "S4/S7::Foo1a", "S4/S7::Foo2a")
    )
  })

  it("handles extensions of base classes", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", contains = "character")
    expect_equal(S4_class_dispatch("Foo1"), c("S4/S7::Foo1", "character"))
  })

  it("handles extensions of S3 classes", {
    on.exit(S4_remove_classes(c("Soo1", "Foo2", "Foo3")))

    setOldClass(c("Soo1", "Soo"))
    setClass("Foo2", contains = "Soo1")
    setClass("Foo3", contains = "Foo2")
    expect_equal(S4_class_dispatch("Foo3"), c("S4/S7::Foo3", "S4/S7::Foo2", "Soo1", "Soo"))
  })

  it("ignores unions", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3")))

    setClass("Foo1", slots = list("x" = "numeric"))
    setClass("Foo2", slots = list("x" = "numeric"))
    setClassUnion("Foo3", c("Foo1", "Foo2"))

    expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
    expect_equal(S4_class_dispatch("Foo2"), "S4/S7::Foo2")
  })

  it("includes virtual classes", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2")))

    setClass("Foo1")
    setClass("Foo2", contains = "Foo1")

    expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
    expect_equal(S4_class_dispatch("Foo2"), c("S4/S7::Foo2", "S4/S7::Foo1"))
  })

  it("captures explicit package name", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", package = "pkg")
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
