test_that("can work with classGenerators", {
  Foo := local_S4_class()
  expect_equal(S4_to_S7_class(Foo), getClass("Foo"))
})

test_that("S4_register registers an S7 class so it can be used with S4 methods", {
  on.exit(S4_remove_classes("S4regS7"))
  S4regS7 := new_class(package = NULL)
  S4_register(S4regS7)
  expect_contains(methods::extends("S4regS7"), c("S4regS7", "S7_object"))
})

test_that("S4_register registers an S3 class so it can be used with S4 methods", {
  on.exit(S4_remove_classes(c("S4regS3a", "S4regS3b")))
  S4_register(new_S3_class(c("S4regS3a", "S4regS3b")))
  # Must not extend S7_object — that was a silent bug pre-fix
  expect_equal(
    methods::extends("S4regS3a"),
    c("S4regS3a", "S4regS3b", "oldClass")
  )
})

test_that("S4_register errors on unsupported inputs", {
  expect_snapshot(error = TRUE, {
    S4_register(1)
    S4_register("foo")
  })
})

test_that("converts S4 base classes to S7 base classes", {
  expect_equal(S4_to_S7_class(getClass("NULL")), NULL)
  expect_equal(S4_to_S7_class(getClass("character")), class_character)
})

test_that("converts S4 unions to S7 unions", {
  Foo1 := local_S4_class(slots = "x")
  Foo2 := local_S4_class(slots = "x")

  Union1 := local_S4_union(c("Foo1", "Foo2"))
  expect_equal(
    S4_to_S7_class(getClass("Union1")),
    new_union(getClass("Foo1"), getClass("Foo2"))
  )

  Foo3 := local_S4_class(slots = "x")
  Union2 := local_S4_union(c("Union1", "Foo3"))
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


test_that("S4_class_dispatch returns name of base class", {
  Foo1 := local_S4_class(slots = list("x" = "numeric"))
  expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
})

test_that("S4_class_dispatch respects single inheritance hierarchy", {
  Foo1 := local_S4_class(slots = list("x" = "numeric"))
  Foo2 := local_S4_class(contains = "Foo1")
  Foo3 := local_S4_class(contains = "Foo2")
  expect_equal(
    S4_class_dispatch("Foo3"),
    c("S4/S7::Foo3", "S4/S7::Foo2", "S4/S7::Foo1")
  )
})

test_that("S4_class_dispatch performs breadth first search for multiple dispatch", {
  Foo1a := local_S4_class(slots = list("x" = "numeric"))
  Foo1b := local_S4_class(contains = "Foo1a")
  Foo2a := local_S4_class(slots = list("x" = "numeric"))
  Foo2b := local_S4_class(contains = "Foo2a")
  Foo3 := local_S4_class(contains = c("Foo1b", "Foo2b"))
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

test_that("S4_class_dispatch handles extensions of base classes", {
  Foo1 := local_S4_class(contains = "character")
  expect_equal(S4_class_dispatch("Foo1"), c("S4/S7::Foo1", "character"))
})

test_that("S4_class_dispatch handles extensions of S3 classes", {
  setOldClass(c("Soo1", "Soo"))
  defer(S4_remove_classes("Soo1"))
  Foo2 := local_S4_class(contains = "Soo1")
  Foo3 := local_S4_class(contains = "Foo2")
  expect_equal(
    S4_class_dispatch("Foo3"),
    c("S4/S7::Foo3", "S4/S7::Foo2", "Soo1", "Soo")
  )
})

test_that("S4_class_dispatch ignores unions", {
  Foo1 := local_S4_class(slots = list("x" = "numeric"))
  Foo2 := local_S4_class(slots = list("x" = "numeric"))
  Foo3 := local_S4_union(c("Foo1", "Foo2"))

  expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
  expect_equal(S4_class_dispatch("Foo2"), "S4/S7::Foo2")
})

test_that("S4_class_dispatch dispatches through the full S3 old-class hierarchy", {
  setOldClass(c("S4OldS3a", "S4OldS3b"))
  defer(S4_remove_classes(c("S4OldS3Child", "S4OldS3a", "S4OldS3b")))
  setClass("S4OldS3Child", contains = "S4OldS3a")

  generic <- new_generic("S4OldS3Generic", "x")
  method(generic, new_S3_class("S4OldS3b")) <- function(x) "S3 parent"

  object <- methods::new("S4OldS3Child")

  expect_contains(obj_dispatch(object), "S4OldS3b")
  expect_equal(generic(object), "S3 parent")
})

test_that("S4_class_dispatch includes virtual classes", {
  Foo1 := local_S4_class()
  Foo2 := local_S4_class(contains = "Foo1")

  expect_equal(S4_class_dispatch("Foo1"), "S4/S7::Foo1")
  expect_equal(S4_class_dispatch("Foo2"), c("S4/S7::Foo2", "S4/S7::Foo1"))
})

test_that("S4_class_dispatch captures explicit package name", {
  Foo1 := local_S4_class(package = "pkg")
  expect_equal(S4_class_dispatch("Foo1"), "S4/pkg::Foo1")
})

test_that("S4_class_dispatch captures implicit package name", {
  env <- new.env()
  env$.packageName <- "mypkg"
  Foo1 := local_S4_class(where = env)
  expect_equal(S4_class_dispatch("Foo1"), "S4/mypkg::Foo1")
})

test_that("S7 class extending S4 class with multiple parents works", {
  on.exit(S4_remove_classes(c(
    "MultiParent1",
    "MultiParent2",
    "MultiChild",
    "S7MultiChild",
    "S7MultiChild2"
  )))

  setClass("MultiParent1", slots = list(x = "numeric"))
  setClass("MultiParent2", slots = list(y = "numeric"))
  setClass("MultiChild", contains = c("MultiParent1", "MultiParent2"))

  S7MultiChild <- new_class(
    "S7MultiChild",
    parent = getClass("MultiChild"),
    properties = list(z = class_numeric),
    package = NULL
  )

  S7MultiChild2 <- new_class(
    "S7MultiChild2",
    parent = S7MultiChild,
    properties = list(w = class_numeric),
    package = NULL
  )

  obj <- S7MultiChild2(x = 1, y = 2, z = 3, w = 4)
  expect_true(S7_inherits(obj, S7MultiChild2))
  expect_true(S7_inherits(obj, S7MultiChild))
  expect_equal(prop(obj, "x"), 1)
  expect_equal(prop(obj, "y"), 2)
  expect_equal(prop(obj, "z"), 3)
  expect_equal(prop(obj, "w"), 4)

  expect_true(methods::is(obj, "MultiParent1"))
  expect_true(methods::is(obj, "MultiParent2"))
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

test_that("S4_package_name errors if originating package can't be found", {
  stats_ns <- asNamespace("stats")

  methods::setGeneric("nonexistent_generic", function(x) {
    standardGeneric("nonexistent_generic")
  })
  on.exit(methods::removeGeneric("nonexistent_generic"))
  mock_S4 <- getGeneric("nonexistent_generic")
  mock_S4@package <- "base"
  expect_snapshot(S4_package_name(mock_S4, stats_ns), error = TRUE)
})
