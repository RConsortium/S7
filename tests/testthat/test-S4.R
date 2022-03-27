test_that("can work with classGenerators", {
  on.exit(S4_remove_classes("Foo", where = globalenv()))
  Foo <- setClass("Foo", where = globalenv())
  expect_equal(S4_to_R7_class(Foo), getClass("Foo"))
})

test_that("converts S4 base classes to R7 base classes", {
  expect_equal(S4_to_R7_class(getClass("NULL")), NULL)
  expect_equal(S4_to_R7_class(getClass("character")), class_character)
})

test_that("converts S4 unions to R7 unions", {
  on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3", "Union1", "Union2"), where = globalenv()))

  setClass("Foo1", slots = "x", where = globalenv())
  setClass("Foo2", slots = "x", where = globalenv())

  setClassUnion("Union1", c("Foo1", "Foo2"), where = globalenv())
  expect_equal(
    S4_to_R7_class(getClass("Union1")),
    new_union(getClass("Foo1"), getClass("Foo2"))
  )

  setClass("Foo3", slots = "x", where = globalenv())
  setClassUnion("Union2", c("Union1", "Foo3"), where = globalenv())
  expect_equal(
    S4_to_R7_class(getClass("Union2")),
    new_union(getClass("Foo1"), getClass("Foo2"), getClass("Foo3"))
  )
})

test_that("converts S4 representation of S3 classes to R7 representation", {
  expect_equal(S4_to_R7_class(getClass("Date")), new_S3_class("Date"), ignore_function_env = TRUE)
})

test_that("errors on non-S4 classes", {
  expect_snapshot(S4_to_R7_class(1), error = TRUE)
})


describe("S4_class_dispatch", {
  it("returns name of base class", {
    on.exit(S4_remove_classes("Foo1", where = globalenv()))
    setClass("Foo1", slots = list("x" = "numeric"), where = globalenv())
    expect_equal(S4_class_dispatch("Foo1"), "S4/Foo1")
  })

  it("respects single inheritance hierarchy", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2","Foo3"), where = globalenv()))

    setClass("Foo1", slots = list("x" = "numeric"), where = globalenv())
    setClass("Foo2", contains = "Foo1", where = globalenv())
    setClass("Foo3", contains = "Foo2", where = globalenv())
    expect_equal(S4_class_dispatch("Foo3"), c("S4/Foo3", "S4/Foo2", "S4/Foo1"))
  })

  it("performs breadth first search for multiple dispatch", {
    on.exit(S4_remove_classes(c("Foo1a", "Foo1b","Foo2a", "Foo2b", "Foo3"), where = globalenv()))
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
    on.exit(S4_remove_classes("Foo1", where = globalenv()))
    setClass("Foo1", contains = "character", where = globalenv())
    expect_equal(S4_class_dispatch("Foo1"), c("S4/Foo1", "character"))
  })

  it("handles extensions of S3 classes", {
    on.exit(S4_remove_classes(c("Soo1", "Foo2", "Foo3"), where = globalenv()))

    setOldClass(c("Soo1", "Soo"), where = globalenv())
    setClass("Foo2", contains = "Soo1", where = globalenv())
    setClass("Foo3", contains = "Foo2", where = globalenv())
    expect_equal(S4_class_dispatch("Foo3"), c("S4/Foo3", "S4/Foo2", "Soo1", "Soo"))
  })

  it("ignores unions", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3"), where = globalenv()))

    setClass("Foo1", slots = list("x" = "numeric"), where = globalenv())
    setClass("Foo2", slots = list("x" = "numeric"), where = globalenv())
    setClassUnion("Foo3", c("Foo1", "Foo2"), where = globalenv())

    expect_equal(S4_class_dispatch("Foo1"), "S4/Foo1")
    expect_equal(S4_class_dispatch("Foo2"), "S4/Foo2")
  })

  it("captures explicit package name", {
    on.exit(S4_remove_classes("Foo1", where = globalenv()))
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

describe("S4 registration", {
  it("can register simple class hierarchy", {
    on.exit(S4_remove_classes(c("foo1", "foo2")))

    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", foo1)

    S4_register(foo1)
    S4_register(foo2)

    expect_s4_class(getClass("foo1"), "classRepresentation")
    expect_s4_class(getClass("foo2"), "classRepresentation")
  })

  it("ties S4 validation to R7 validation", {
    on.exit(S4_remove_classes(c("foo1", "Foo2")))

    foo1 <- new_class("foo1",
      parent = class_integer,
      validator = function(self) {
        if (R7_data(self) < 0) "Must be positive"
      }
    )
    # Create invalid object
    R7_obj <- foo1(1L)
    R7_obj[[1]] <- -1L

    S4_register(foo1)
    Foo2 <- setClass("Foo2", slots = list(x = "foo1"))
    S4_obj <- Foo2(x = R7_obj)

    expect_error(validObject(S4_obj, complete = TRUE), "Must be positive")
  })

  it("can register slots", {
    on.exit(S4_remove_classes(c("foo1", "foo2")))

    foo1 <- new_class("foo1", properties = list(x = class_integer))
    foo2 <- new_class("foo2", foo1, properties = list(y = class_character))

    S4_register(foo1)
    S4_register(foo2)
    expect_equal(getClass("foo1")@slots$x, structure("integer", package = "methods"))
    expect_equal(getClass("foo2")@slots$x, structure("integer", package = "methods"))
    expect_equal(getClass("foo2")@slots$y, structure("character", package = "methods"))
  })

  it("translates double to numeric", {
    on.exit(S4_remove_classes("foo1"))
    foo1 <- new_class("foo1",
      parent = class_double,
      properties = list(x = class_double)
    )
    S4_register(foo1)

    obj <- new("foo1")
    expect_type(obj, "double")
    expect_type(slot(obj, "x"), "double")
  })

  it("checks its inputs", {
    expect_snapshot(S4_register("x"), error = TRUE)
  })
})
