test_that("can work with classGenerators", {
  on.exit(S4_remove_classes("Foo"))
  Foo <- setClass("Foo")
  expect_equal(S4_to_S7_class(Foo), getClass("Foo"))
})

describe("S4_register", {
  it("registers an S7 class so it can be used with S4 methods", {
    on.exit(S4_remove_classes("S4regS7"))
    S4regS7 <- new_class("S4regS7", package = NULL)
    S4_register(S4regS7)
    expect_contains(methods::extends("S4regS7"), c("S4regS7", "S7_object"))
  })

  it("registers an S3 class so it can be used with S4 methods", {
    on.exit(S4_remove_classes(c("S4regS3a", "S4regS3b")))
    S4_register(new_S3_class(c("S4regS3a", "S4regS3b")))
    # Must not extend S7_object — that was a silent bug pre-fix
    expect_equal(
      methods::extends("S4regS3a"),
      c("S4regS3a", "S4regS3b", "oldClass")
    )
  })

  it("errors on unsupported inputs", {
    expect_snapshot(error = TRUE, {
      S4_register(1)
      S4_register("foo")
    })
  })
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
  expect_equal(
    S4_to_S7_class(getClass("Date")),
    class_Date,
    ignore_function_env = TRUE
  )
})

test_that("errors on non-S4 classes", {
  expect_snapshot(S4_to_S7_class(1), error = TRUE)
})

test_that("S7 classes can extend S4 classes", {
  on.exit(S4_remove_classes(c("Parent", "Child")))
  setClass("Parent", slots = list(x = "numeric"))

  Child <- new_class(
    "Child",
    parent = getClass("Parent"),
    properties = list(y = class_character),
    package = NULL
  )
  child <- Child(x = 1, y = "a")

  expect_false(isS4(child))
  expect_true(S7_inherits(child, Child))
  expect_equal(prop_names(child), c("x", "y"))
  expect_equal(prop(child, "x"), 1)
  expect_equal(prop(child, "y"), "a")

  prop(child, "x") <- 2
  prop(child, "y") <- "b"
  expect_equal(prop(child, "x"), 2)
  expect_equal(prop(child, "y"), "b")

  expect_true(methods::is(child, "Parent"))
  expect_true(methods::validObject(child))
  expect_equal(methods::slotNames("Child"), c("x", "y", ".S3Class"))
  expect_equal(methods::slot(child, "x"), 2)
  expect_equal(methods::slot(child, "y"), "b")

  child <- methods::initialize(child, x = 3, y = "c")
  expect_equal(prop(child, "x"), 3)
  expect_equal(prop(child, "y"), "c")
  expect_true(methods::validObject(child))

  child <- methods::initialize(child, Child(x = 4, y = "d"), y = "e")
  expect_equal(prop(child, "x"), 4)
  expect_equal(prop(child, "y"), "e")

  parent <- methods::new("Parent", x = 5)
  child <- methods::initialize(child, parent, y = "f")
  expect_equal(prop(child, "x"), 5)
  expect_equal(prop(child, "y"), "f")

  child <- methods::initialize(child, x = 6, x = 7)
  expect_equal(prop(child, "x"), 7)

  expect_error(methods::initialize(child, x = "x"), "invalid")
  expect_error(methods::initialize(child, z = 1), "Can't find property")

  expect_error(Child(x = "x", y = "a"))
})

test_that("S4 initialize supports S3 data parts", {
  on.exit(S4_remove_classes(c("ParentNum", "ChildNum")))
  setClass("ParentNum", contains = "numeric", slots = list(y = "character"))

  ChildNum <- new_class(
    "ChildNum",
    parent = getClass("ParentNum"),
    properties = list(z = class_integer),
    package = NULL
  )
  child <- ChildNum(y = "a", z = 1L)

  child <- methods::initialize(child, 2.5, y = "b")
  expect_equal(as.vector(child), 2.5)
  expect_equal(prop(child, ".Data"), 2.5)
  expect_equal(prop(child, "y"), "b")
  expect_true(methods::validObject(child))

  child <- methods::initialize(child, .Data = 3.5)
  expect_equal(as.vector(child), 3.5)
  expect_true(methods::validObject(child))
})

test_that("S4_register uses S7 properties as known S4 attributes", {
  on.exit(S4_remove_classes("Foo"))
  Foo <- new_class("Foo", properties = list(x = class_numeric), package = NULL)
  foo <- Foo(x = 1)

  S4_register(Foo)
  expect_true(methods::validObject(foo))
  expect_equal(methods::slot(foo, "x"), 1)

  attr(foo, "x") <- "x"
  expect_error(methods::validObject(foo), "invalid object")
})

test_that("S4 initialize uses S7 property setters", {
  on.exit(S4_remove_classes(c("Parent2", "Child2")))
  setClass("Parent2", slots = list(x = "numeric"))

  Child2 <- new_class(
    "Child2",
    parent = getClass("Parent2"),
    properties = list(
      y = new_property(class_character, setter = function(self, value) {
        attr(self, "setter_called") <- TRUE
        attr(self, "y") <- value
        self
      })
    ),
    package = NULL
  )

  child <- methods::initialize(Child2(x = 1, y = "a"), y = "b")
  expect_equal(prop(child, "y"), "b")
  expect_true(attr(child, "setter_called"))
})


describe("S4_class_dispatch", {
  it("returns name of base class", {
    on.exit(S4_remove_classes("Foo1"))
    setClass("Foo1", slots = list("x" = "numeric"))
    expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
  })

  it("respects single inheritance hierarchy", {
    on.exit(S4_remove_classes(c("Foo1", "Foo2", "Foo3")))

    setClass("Foo1", slots = list("x" = "numeric"))
    setClass("Foo2", contains = "Foo1")
    setClass("Foo3", contains = "Foo2")
    expect_equal(
      S4_class_dispatch("Foo3"),
      c("S4/S7::Foo3", "S4/S7::Foo2", "S4/S7::Foo1")
    )
  })

  it("performs breadth first search for multiple dispatch", {
    on.exit(S4_remove_classes(c("Foo1a", "Foo1b", "Foo2a", "Foo2b", "Foo3")))
    setClass("Foo1a", slots = list("x" = "numeric"))
    setClass("Foo1b", contains = "Foo1a")
    setClass("Foo2a", slots = list("x" = "numeric"))
    setClass("Foo2b", contains = "Foo2a")
    setClass("Foo3", contains = c("Foo1b", "Foo2b"))
    expect_equal(
      S4_class_dispatch("Foo3"),
      c(
        "S4/S7::Foo3",
        "S4/S7::Foo1b",
        "S4/S7::Foo2b",
        "S4/S7::Foo1a",
        "S4/S7::Foo2a"
      )
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
    expect_equal(
      S4_class_dispatch("Foo3"),
      c("S4/S7::Foo3", "S4/S7::Foo2", "Soo1", "Soo")
    )
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

test_that("S4_package_name resolves S4 package name correctly in all cases", {
  stats_ns <- asNamespace("stats")

  # Case 1: Generic in declared package
  show_S4 <- getGeneric("show")
  expect_equal(S4_package_name(show_S4, stats_ns), "methods")

  # Case 2: Current package bypass
  dummy_S4 <- getGeneric("show")
  dummy_S4@package <- "stats"
  expect_equal(S4_package_name(dummy_S4, stats_ns), "stats")

  # Case 3: Originating package in namespace imports
  methods::setGeneric("axTicks")
  on.exit(methods::removeGeneric("axTicks"))
  mock_S4 <- getGeneric("axTicks")
  mock_S4@package <- "base"
  expect_equal(S4_package_name(mock_S4, stats_ns), "graphics")
})
